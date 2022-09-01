package code.playground.akka.persistence

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, SupervisorStrategy}
import akka.pattern.StatusReply
import akka.persistence.typed.PersistenceId
import akka.persistence.typed.scaladsl.{Effect, EventSourcedBehavior, RetentionCriteria}
import code.playground.akka.serialization.CborSerializable
import com.typesafe.config.{Config, ConfigFactory}

import java.time.Instant
import scala.concurrent.duration._

object SimpleShoppingCartApp {

  object ShoppingCart {

    /**
     * The current state held by the persistent entity.
     */
    final case class State(items: Map[String, Int], checkoutDate: Option[Instant]) extends CborSerializable {

      def isCheckedOut: Boolean =
        checkoutDate.isDefined

      def hasItem(itemId: String): Boolean =
        items.contains(itemId)

      def isEmpty: Boolean =
        items.isEmpty

      def updateItem(itemId: String, quantity: Int): State = {
        quantity match {
          case 0 => copy(items = items - itemId)
          case _ => copy(items = items + (itemId -> quantity))
        }
      }

      def removeItem(itemId: String): State =
        copy(items = items - itemId)

      def checkout(now: Instant): State =
        copy(checkoutDate = Some(now))

      def toSummary: Summary =
        Summary(items, isCheckedOut)
    }

    object State {
      val empty = State(items = Map.empty, checkoutDate = None)
    }

    /**
     * This interface defines all the commands that the ShoppingCart persistent actor supports.
     */
    sealed trait Command extends CborSerializable

    /**
     * A command to add an item to the cart.
     *
     * It can reply with `StatusReply[Summary]`, which is sent back to the caller when
     * all the events emitted by this command are successfully persisted.
     */
    final case class AddItem(itemId: String, quantity: Int, replyTo: ActorRef[StatusReply[Summary]]) extends Command

    /**
     * A command to remove an item from the cart.
     */
    final case class RemoveItem(itemId: String, replyTo: ActorRef[StatusReply[Summary]]) extends Command

    /**
     * A command to adjust the quantity of an item in the cart.
     */
    final case class AdjustItemQuantity(itemId: String, quantity: Int, replyTo: ActorRef[StatusReply[Summary]]) extends Command

    /**
     * A command to checkout the shopping cart.
     */
    final case class Checkout(replyTo: ActorRef[StatusReply[Summary]]) extends Command

    /**
     * A command to get the current state of the shopping cart.
     */
    final case class Get(replyTo: ActorRef[StatusReply[Summary]]) extends Command

    /**
     * Summary of the shopping cart state, used in reply messages.
     */
    final case class Summary(items: Map[String, Int], checkedOut: Boolean) extends CborSerializable

    /**
     * This interface defines all the events that the ShoppingCart supports.
     */
    sealed trait Event extends CborSerializable {
      def cartId: String
    }

    final case class ItemAdded(cartId: String, itemId: String, quantity: Int) extends Event

    final case class ItemRemoved(cartId: String, itemId: String) extends Event

    final case class ItemQuantityAdjusted(cartId: String, itemId: String, newQuantity: Int) extends Event

    final case class CheckedOut(cartId: String, eventTime: Instant) extends Event

    def apply(cartId: String): Behavior[Command] = {
      EventSourcedBehavior[Command, Event, State](
        PersistenceId("ShoppingCart", cartId),
        State.empty,
        (state, command) =>
          //The shopping cart behavior changes if it's checked out or not.
          // The commands are handled differently for each case.
          if (state.isCheckedOut) checkedOutShoppingCart(cartId, state, command)
          else openShoppingCart(cartId, state, command),
        (state, event) => handleEvent(state, event))
        .withRetention(RetentionCriteria.snapshotEvery(numberOfEvents = 100, keepNSnapshots = 3))
        .onPersistFailure(SupervisorStrategy.restartWithBackoff(200.millis, 5.seconds, 0.1))
    }

    private def openShoppingCart(cartId: String, state: State, command: Command): Effect[Event, State] =
      command match {
        case AddItem(itemId, quantity, replyTo) =>
          if (state.hasItem(itemId)) {
            replyTo ! StatusReply.Error(s"Item '$itemId' was already added to this shopping cart")
            Effect.none
          } else if (quantity <= 0) {
            replyTo ! StatusReply.Error("Quantity must be greater than zero")
            Effect.none
          } else {
            Effect
              .persist(ItemAdded(cartId, itemId, quantity))
              .thenRun(updatedCart => replyTo ! StatusReply.Success(updatedCart.toSummary))
          }

        case RemoveItem(itemId, replyTo) =>
          if (state.hasItem(itemId)) {
            Effect.persist(ItemRemoved(cartId, itemId)).thenRun(updatedCart => replyTo ! StatusReply.Success(updatedCart.toSummary))
          } else {
            replyTo ! StatusReply.Success(state.toSummary) // removing an item is idempotent
            Effect.none
          }

        case AdjustItemQuantity(itemId, quantity, replyTo) =>
          if (quantity <= 0) {
            replyTo ! StatusReply.Error("Quantity must be greater than zero")
            Effect.none
          } else if (state.hasItem(itemId)) {
            Effect
              .persist(ItemQuantityAdjusted(cartId, itemId, quantity))
              .thenRun(updatedCart => replyTo ! StatusReply.Success(updatedCart.toSummary))
          } else {
            replyTo ! StatusReply.Error(s"Cannot adjust quantity for item '$itemId'. Item not present on cart")
            Effect.none
          }

        case Checkout(replyTo) =>
          if (state.isEmpty) {
            replyTo ! StatusReply.Error("Cannot checkout an empty shopping cart")
            Effect.none
          } else {
            Effect
              .persist(CheckedOut(cartId, Instant.now()))
              .thenRun(updatedCart => replyTo ! StatusReply.Success(updatedCart.toSummary))
          }

        case Get(replyTo) =>
          replyTo ! StatusReply.Success(state.toSummary)
          Effect.none
      }

    private def checkedOutShoppingCart(cartId: String, state: State, command: Command): Effect[Event, State] =
      command match {
        case Get(replyTo) =>
          replyTo ! StatusReply.success(state.toSummary)
          Effect.none
        case cmd: AddItem =>
          cmd.replyTo ! StatusReply.Error("Can't add an item to an already checked out shopping cart")
          Effect.none
        case cmd: RemoveItem =>
          cmd.replyTo ! StatusReply.Error("Can't remove an item from an already checked out shopping cart")
          Effect.none
        case cmd: AdjustItemQuantity =>
          cmd.replyTo ! StatusReply.Error("Can't adjust item on an already checked out shopping cart")
          Effect.none
        case cmd: Checkout =>
          cmd.replyTo ! StatusReply.Error("Can't checkout already checked out shopping cart")
          Effect.none
      }

    private def handleEvent(state: State, event: Event) = {
      event match {
        case ItemAdded(_, itemId, quantity) => state.updateItem(itemId, quantity)
        case ItemRemoved(_, itemId) => state.removeItem(itemId)
        case ItemQuantityAdjusted(_, itemId, quantity) =>
          state.updateItem(itemId, quantity)
        case CheckedOut(_, eventTime) => state.checkout(eventTime)
      }
    }

  }


  private def getConfig(): Config =
    ConfigFactory.parseString(
      s"""
      akka {
        loglevel = DEBUG

        actor {
          serialization-bindings {
            "com.behsa.mediation.playground.akka.serialization.CborSerializable" = jackson-cbor
          }
        }

        persistence {
          # inmem only for tests
          journal.plugin = "akka.persistence.journal.inmem"
          snapshot-store.plugin = "akka.persistence.snapshot-store.local"
          snapshot-store.local.dir = "target/snapshot"
        }
      }
  """).withFallback(ConfigFactory.load())

  object CustomerActor {

    import ShoppingCart._

    def apply(): Behavior[StatusReply[Summary]] = Behaviors.setup { context =>

      import ShoppingCart._
      val shoppingCart: ActorSystem[Command] = ActorSystem[Command](ShoppingCart("cart-100"), "shopping-cart-system", getConfig())
      shoppingCart ! AddItem("item 1", 11, context.self)
      shoppingCart ! AddItem("item 2", 21, context.self)
      shoppingCart ! AddItem("item 3", 5, context.self)
      shoppingCart ! RemoveItem("item 2", context.self)

      shoppingCart ! Get(context.self)

      Behaviors.receiveMessage {
        case StatusReply.Success(result: Summary) =>
          println(s"The cart items: ${result.items}")
          Behaviors.same
        case StatusReply.Error(e) =>
          println(s"got an error! $e")
          Behaviors.same
      }
    }

  }

  def main(args: Array[String]): Unit = {
    import ShoppingCart._

    ActorSystem[StatusReply[Summary]](CustomerActor(), "customer-system", getConfig())
  }

}

package code.playground.akka.actor

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, DispatcherSelector, Dispatchers}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object AkkaMessageAdaptation {

  object StoreDomain {
    case class Product(name: String, price: Double) // never use double for money
  }

  object ShoppingCart {
    import StoreDomain._

    sealed trait Request
    case class GetCurrentCart(cartId: String, replyTo: ActorRef[Response]) extends Request
    // some others

    sealed trait Response
    case class CurrentCart(cartId: String, items: List[Product]) extends Response
    // some others

    // NEW: a dummy database holding all the current shopping carts
    val db: Map[String, List[Product]] = Map {
      "123-abc-456" -> List(Product("iPhone", 7000), Product("selfie stick", 30))
    }

    // NEW: a dummy shopping cart fetching things from the internal in-memory "database"/map
    def apply(): Behavior[Request] = Behaviors.receiveMessage {
      case GetCurrentCart(cartId, replyTo) =>
        replyTo ! CurrentCart(cartId, db(cartId))
        Behaviors.same
    }
  }

  object Checkout {
    import ShoppingCart._

    sealed trait Request
    final case class InspectSummary(cartId: String, replyTo: ActorRef[Response]) extends Request
    // some others

    // message wrapper that can translate from the outer (backend) actor's responses to my own useful data structures
    private final case class WrappedSCResponse(response: ShoppingCart.Response) extends Request

    sealed trait Response
    final case class Summary(cartId: String, amount: Double) extends Response

    def apply(shoppingCart: ActorRef[ShoppingCart.Request]): Behavior[Request] =
      Behaviors.setup[Request] { context =>
        // adapter goes here
        val responseMapper: ActorRef[ShoppingCart.Response] =
          context.messageAdapter(rsp => WrappedSCResponse(rsp))

        // checkout behavior's logic
        def handlingCheckouts(checkoutsInProgress: Map[String, ActorRef[Response]]): Behavior[Request] = {
          Behaviors.receiveMessage[Request] {
            // message from customer - query the shopping cart
            // the recipient of that response is my message adapter
            case InspectSummary(cartId, replyTo) =>
              shoppingCart ! ShoppingCart.GetCurrentCart(cartId, responseMapper) // <--- message adapter here
              handlingCheckouts(checkoutsInProgress + (cartId -> replyTo))

            // the wrapped message from my adapter: deal with the Shopping Cart's response here
            case WrappedSCResponse(resp) =>
              resp match {
                case CurrentCart(cartId, items) =>
                  val summary = Summary(cartId, items.map(_.price).sum)
                  val customer = checkoutsInProgress(cartId)
                  customer ! summary
                  Behaviors.same

                // handle other potential responses from the ShoppingCart actor here
              }

          }
        }

        handlingCheckouts(checkoutsInProgress = Map())
      }
  }

  // NEW - a main app with an actor system spawning a customer, checkout and shopping cart actor
  def main(args: Array[String]): Unit = {
    import Checkout._

    val rootBehavior: Behavior[Any] = Behaviors.setup { context =>
      val shoppingCart = context.spawn(ShoppingCart(), "shopping-cart")

      // simple customer actor displaying the total amount due
      val customer = context.spawn(Behaviors.receiveMessage[Response] {
        case Summary(_, amount) =>
          println(s"Total to pay: $amount - pay by card below.")
          Behaviors.same
      }, "customer")

      val checkout = context.spawn(Checkout(shoppingCart), "checkout")

      // trigger an interaction
      checkout ! InspectSummary("123-abc-456", customer)

      // no behavior for the actor system
      Behaviors.empty
    }

    // setup/teardown
    val system = ActorSystem(rootBehavior, "main-app")
    implicit val ec: ExecutionContext = system.dispatchers.lookup(DispatcherSelector.default)
    system.scheduler.scheduleOnce(1.second, () => system.terminate())
  }
}
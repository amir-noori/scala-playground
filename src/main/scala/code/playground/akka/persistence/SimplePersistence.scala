package code.playground.akka.persistence

import akka.actor.typed.Behavior
import akka.persistence.typed.PersistenceId
import akka.persistence.typed.scaladsl.{Effect, EventSourcedBehavior}

object SimplePersistence extends App {


  object SimplePersistenceBehavior {

    sealed trait Command

    final case class Add(data: String) extends Command

    case object Clear extends Command

    sealed trait Event

    final case class Added(data: String) extends Event

    case object Cleared extends Event

    final case class State(history: List[String] = Nil)

    val commandHandler: (State, Command) => Effect[Event, State] = { (state, command) =>
      command match {
        case Add(data) => Effect.persist(Added(data))
        case Clear => Effect.persist(Cleared)
      }
    }

    val eventHandler: (State, Event) => State = { (state, event) =>
      event match {
        case Added(data) => state.copy((data :: state.history).take(5))
        case Cleared => State(Nil)
      }
    }

    def apply(): Behavior[Command] = EventSourcedBehavior[Command, Event, State](
      persistenceId = PersistenceId.ofUniqueId("SIMPLE_PERSISTENCE_BEHAVIOR"),
      emptyState = State(Nil),
      commandHandler = (state, cmd) => commandHandler(state, cmd),
      eventHandler = (state, event) => eventHandler(state, event)
    )
  }


}

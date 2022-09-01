package code.playground.akka.actor

import akka.actor.typed.{ActorSystem, Behavior, SupervisorStrategy}
import akka.actor.typed.scaladsl.{Behaviors, Routers}
import RouterApp.Worker.DoLog

object RouterApp extends App {


  object Worker {

    sealed trait Command

    case class DoLog(text: String) extends Command

    def apply(): Behavior[Command] = Behaviors.setup { context =>
      println(s"Starting worker")

      Behaviors.receiveMessage {
        case DoLog(text) =>
          println(s"Logging: $text")
          Behaviors.same
      }

    }

  }


  object MyRouter {

    def apply(): Behavior[Unit] = Behaviors.setup[Unit] { ctx =>

      val pool = Routers.pool(poolSize = 4) {
        // make sure the workers are restarted if they fail
        Behaviors.supervise(Worker()).onFailure[Exception](SupervisorStrategy.restart)
      }
      val router = ctx.spawn(pool, "worker-pool")

      (0 to 10).foreach { n =>
        router ! Worker.DoLog(s"msg $n")
      }

      val poolWithBroadcast = pool.withBroadcastPredicate(_.isInstanceOf[DoLog])
      val routerWithBroadcast = ctx.spawn(poolWithBroadcast, "pool-with-broadcast")
      //this will be sent to all 4 routees
      routerWithBroadcast ! DoLog("this message is a broadcast")
      Behaviors.empty
    }

  }


  ActorSystem(MyRouter(), "my-router")

}

package code.playground.akka.actor

import akka.NotUsed
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Terminated}


object ActorPool extends App {

  sealed trait Request

  sealed trait Response

  final case class ConnectRequest() extends Request

  final case class DisconnectRequest() extends Request

  final case class BaseRequest(data: String, id: Int) extends Request

  final case class BaseResponse(data: String) extends Response

  sealed trait ServerState

  final case class Listening() extends ServerState


  sealed trait ClientState

  final case class Disconnected() extends ClientState

  final case class Connected() extends ClientState


  object Server {

    def apply(): Behavior[Request] =
      Behaviors.receive { (context, state) =>
        state match {
          case ConnectRequest() =>
            println("connection established with client")
          case BaseRequest(data, id) =>
            if (id % 2 == 0) {
              println(s"even data received from client: $data, $id. thread: ${Thread.currentThread().getId}")
            } else {
              println(s"odd! lets wait a second, thread: ${Thread.currentThread().getId}")
              longComputation()
              println(s"odd data received from client: $data, $id")
            }

        }
        Behaviors.same
      }


    def longComputation(): Unit = {
      0 to 100000000 foreach { i =>
        val x = i + 10 * 222 - 122
      }
    }

  }

  object Client {

  }

  object Main {
    def apply(): Behavior[NotUsed] =
      Behaviors.setup { context =>
        val POOL_SIZE = 1000
        var serverPool = List[ActorRef[Request]]()
        1 to POOL_SIZE foreach { i =>
          val server = context.spawn(Server(), "server_" + i)
          serverPool = serverPool.appended(server)
          context.watch(server)
        }

        1 to 1000 foreach { i =>
          val j = if (i < POOL_SIZE) i else i % POOL_SIZE
          println(s"lets call server $i, $j")
          serverPool(j) ! BaseRequest("test data", i)
        }

        Behaviors.same
      }

    def main(args: Array[String]): Unit = {
      ActorSystem(Main(), "client_server_system")
    }

  }

  Main.main(Array())

}

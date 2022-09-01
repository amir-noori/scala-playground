package code.playground.akka.stream

import akka.actor.Cancellable
import akka.actor.typed.ActorRef
import akka.{Done, NotUsed}

import scala.util.{Failure, Success}


object Flows extends App {

  import akka.actor.ActorSystem
  import akka.stream.scaladsl._
  import scala.concurrent._
  import akka.stream._


  implicit val actorSystem: ActorSystem = ActorSystem("flows")
  implicit val executionContext = actorSystem.dispatcher

  // explicitly wire up source, flow and sink together
  val done: Future[Done] = Source(1 to 100)
    .via(Flow[Int].map(x => x + 100).filter(x => x % 2 == 0))
    .runWith(Sink.foreach(println(_)))


  done.onComplete(_ => {
    actorSystem.terminate()
  })

}

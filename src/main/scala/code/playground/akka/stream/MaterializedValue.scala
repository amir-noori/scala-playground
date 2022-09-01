package code.playground.akka.stream

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Flow, Keep, RunnableGraph, Sink, Source}
import akka.stream.{KillSwitch, KillSwitches}
import akka.{Done, NotUsed}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

object MaterializedValue extends App {

  implicit val system: ActorSystem = ActorSystem("Pre-Materialization-Actor")
  implicit val executionContext: ExecutionContext = system.dispatcher

  //  val promise = Promise[Done]()
  //  val stream: RunnableGraph[NotUsed] = Source.repeat("Hello world")
  //    .take(3)
  //    .to {
  //      Sink.foreach(println).mapMaterializedValue { done =>
  //        done.onComplete {
  //          case Success(_) =>
  //            println("success")
  //            promise.success(Done)
  //          case Failure(ex) =>
  //            println("fail")
  //            promise.failure(ex)
  //        }
  //      }
  //    }
  //
  //  stream.run()
  //  promise.future.onComplete {
  //    case Success(value) => println(s"result value: $value")
  //  }


  trait ControlInterface {
    def stop(): Unit
  }

  class ControlInterfaceImpl(killSwitch: KillSwitch)
    extends ControlInterface {
    def stop(): Unit = killSwitch.shutdown()
  }

  val source: Source[Int, ControlInterface] =
    Source.fromIterator(() => Iterator.from(1))
      .throttle(1, 500.millis)
      .viaMat(KillSwitches.single)(Keep.right)
      .mapMaterializedValue(s => new ControlInterfaceImpl(s))

  val flow1: Flow[Int, Int, NotUsed] = Flow[Int].map(x => x * x)
  val flow2: Flow[Int, Int, NotUsed] = Flow[Int].map(x => x + 1)
  val sink: Sink[Int, Future[Done]] = Sink.foreach(println)

  val (control, doneF): (ControlInterface, Future[Done]) = source
    .viaMat(flow1)(Keep.left) // explicitly specifying Keep.left
    .via(flow2) // implicitly specifying Keep.left
    .toMat(sink)(Keep.both) // collecting both values
    .run()

  system.scheduler.scheduleOnce(10.seconds) {
    println("Stopping the source")
    control.stop()
  }

  doneF.onComplete { result =>
    result match {
      case Success(_) =>
        println("The stream completed successfully")
      case Failure(ex) =>
        println(s"The stream failed with exception: ${ex}")
    }
    system.terminate()
  }

}

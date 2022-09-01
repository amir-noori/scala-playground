package code.playground.akka.stream

import akka.NotUsed
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.stream.ClosedShape
import akka.stream.scaladsl.GraphDSL.Implicits.{SourceArrow, port2flow}
import akka.stream.scaladsl.{Broadcast, Flow, GraphDSL, RunnableGraph, Sink, Source, Zip}

object SimpleGraph extends App {

  implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "graph-actor-sys")

  val input = Source(1 to 100)
  val incrementer = Flow[Int].map(x => x + 1)
  val multiplier = Flow[Int].map(x => x * 10)
  val subtractor = Flow[Int].map(x => x - 1)
  val output = Sink.foreach[(Int, Int)](println)

  val graph = RunnableGraph.fromGraph(
    GraphDSL.create() { implicit builder: GraphDSL.Builder[NotUsed] =>

      val broadcast = builder.add(Broadcast[Int](2)) // fan-out
      val zip = builder.add(Zip[Int, Int]) // fan-in

      /*
              0 ---> incrementer ---> zip.in0
        input |                              | ---> zip.out ---> output
              0 ---> multiplier  ---> zip.in1
       */
      input ~> broadcast
      broadcast.out(0) ~> incrementer ~> zip.in0
      broadcast.out(1) ~> multiplier ~> zip.in1
      zip.out ~> output

      ClosedShape

    }
  ).run()

}

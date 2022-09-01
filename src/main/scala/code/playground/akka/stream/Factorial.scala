package code.playground.akka.stream

object Factorial extends App {

  import akka.stream._
  import akka.stream.scaladsl._

  import akka.{ Done, NotUsed }
  import akka.actor.ActorSystem
  import akka.util.ByteString
  import scala.concurrent._
  import scala.concurrent.duration._
  import java.nio.file.Paths


  implicit val system: ActorSystem = ActorSystem("QuickStart")

  // start a source to emit integers 1 to 100
  val source: Source[Int, NotUsed] = Source(1 to 100)

//  source.runForeach(x => println("number: " + x))
//  source.runForeach(x => println("number_: " + x))

  val factorials = source.scan(BigInt(1))((acc, next) => acc * next)

  val result: Future[IOResult] =
    factorials
      .map(num => ByteString(s"$num\n"))
      .runWith(FileIO.toPath(Paths.get("factorials.txt")))


  def lineSink(filename: String): Sink[String, Future[IOResult]] =
    Flow[String]
      .map(s => ByteString(s + "\n"))
      .toMat(FileIO.toPath(Paths.get(filename)))(Keep.right)

  factorials
    .map(_.toString)
    .runWith(lineSink("factorial2.txt"))

  factorials
    .zipWith(Source(0 to 5))((num, idx) => s"$idx! = $num")
    .throttle(1, 1.second)
    .runForeach(println)


//  implicit val executionContext = system.dispatcher
//
//  result.onComplete(_ => {
//    println(s"result: $result")
//    system.terminate()
//  })
}

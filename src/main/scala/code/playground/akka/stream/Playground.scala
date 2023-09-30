package code.playground.akka.stream


object Playground {

  import akka.stream._
  import akka.stream.scaladsl._

  import akka.{ Done, NotUsed }
  import akka.actor.ActorSystem
  import akka.util.ByteString
  import scala.concurrent._
  import scala.concurrent.duration._
  import java.nio.file.Paths

  def main(args: Array[String]): Unit = {

    implicit val system: ActorSystem = ActorSystem("QuickStart")

    val source: Source[Int, NotUsed] = Source(1 to 100)
    val factorials = source.scan(BigInt(1))((acc, next) => acc * next)
    val result: Future[IOResult] =
      factorials.map(num => ByteString(s"$num\n")).runWith(FileIO.toPath(Paths.get("factorials.txt")))

    implicit val ec: ExecutionContextExecutor = system.dispatcher
    result.onComplete(_ => system.terminate())

  }

}

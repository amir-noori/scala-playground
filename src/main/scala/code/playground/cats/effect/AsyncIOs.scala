package code.playground.cats.effect

import cats.effect.kernel.{Fiber, Outcome}
import cats.effect.{ExitCode, IO, IOApp}

object AsyncIOs extends IOApp {

  def produceInt: IO[Int] = IO(100)

  def produceStr: IO[String] = IO("data")

  def createFiber: Fiber[IO, Throwable, String] = ???

  implicit class DebugHelper[A](io: IO[A]) {
    def debug: IO[A] = io.map { value =>
      println(s"[${Thread.currentThread().getName}] value -> $value")
      value
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      i <- produceInt.debug.start
      s <- produceStr.debug.start
      x <- s.join
      result <- x match {
        case Outcome.Succeeded(result) => IO(s"$result")
        case _ => IO("hmm!")
      }
      _ <- IO.println(s"result: $result")
    } yield ExitCode.Success
  }
}

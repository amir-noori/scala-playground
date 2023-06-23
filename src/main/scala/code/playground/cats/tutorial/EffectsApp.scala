package code.playground.cats.tutorial

import cats.effect.{ExitCode, IO, IOApp}

object EffectsApp extends IOApp {

  val logic = for {
    i1 <- IO(10)
    i2 <- IO(20)
  } yield i1 + i2

  override def run(args: List[String]): IO[ExitCode] =
    logic
      .map(result => result * 2)
      .map(println)
      .as(ExitCode.Success)
}

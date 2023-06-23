package code.playground.cats.tutorial

import cats.{Parallel, Traverse}
import cats.effect.IO.Par
import cats.effect.{ExitCode, IO, IOApp}
import code.playground.cats.tutorial.EffectsUtil.getThreadInfo

//noinspection TypeAnnotation
object IOPar extends IOApp {

  val firstIO = IO {
    println(getThreadInfo)
    10
  }

  val secondIO = IO {
    println(getThreadInfo)
    20
  }

  val logic = for {
    v1 <- firstIO
    v2 <- secondIO
  } yield v1 + v2


  val firstParIO: Par[Int] = Parallel[IO].parallel(firstIO)
  val secondParIO: Par[Int] = Parallel[IO].parallel(secondIO)

  import cats.effect.implicits._
  import cats.implicits.catsSyntaxTuple2Semigroupal // for using mapN
  val parLogic: Par[Int] = (firstParIO, secondParIO).mapN((a, b) => a + b)

  import cats.syntax.parallel._
  def sequence[F[_]: Traverse, A](values: F[IO[A]]): IO[F[A]] =
    Traverse[F].traverse(values)(a => a)

  def sequencePar[F[_]: Traverse, A](values: F[IO[A]]): IO[F[A]] =
    values.parTraverse(a => a)


  override def run(args: List[String]): IO[ExitCode] = {
//    logic.map(println).as(ExitCode.Success)
    Parallel[IO].sequential(parLogic).as(ExitCode.Success)
  }
}

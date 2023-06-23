package code.playground.cats.tutorial

import cats.effect.IO
import cats.effect.unsafe.implicits.global

import scala.util.{Failure, Success, Try}

object IOErrorHandling {


  def option2IO[A](op: Option[A])(ifEmpty: Throwable): IO[A] = op match {
    case Some(value) => IO(value)
    case None => IO.raiseError(ifEmpty)
  }

  def try2IO[A](t: Try[A]): IO[A] = t match {
    case Success(v) => IO(v)
    case Failure(ex) => IO.raiseError(ex)
  }

  def either2IO[A](e: Either[Throwable, A]): IO[A] = e match {
    case Right(v) => IO(v)
    case Left(tr) => IO.raiseError(tr)
  }

  def handleIOError[A](io: IO[A])(errorHandler: Throwable => A): IO[A] =
    io.redeem(errorHandler, a => a)

  def handleIOErrorWith[A](io: IO[A])(errorHandler: Throwable => IO[A]): IO[A] =
    io.redeemWith(errorHandler, IO(_))

  def main(args: Array[String]): Unit = {

  }

}

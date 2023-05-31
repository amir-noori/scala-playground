package code.playground.fpscala

import scala.io.StdIn.readLine
import Ch11.Monad

import scala.annotation.tailrec

object Ch13 {

  trait IO[+A] { self =>
    def run: A

    def map[B](f: A => B): IO[B] = new IO[B] {
      def run: B = f(self.run)
    }

    def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
      def run: B = f(self.run).run
    }
  }

  object IO extends Monad[IO] {
    def unit[A](a: => A): IO[A] = new IO[A] {
      def run: A = a
    }

    def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f

    def apply[A](a: => A): IO[A] = unit(a)
  }


  def test(): IO[Unit] = {

    def fahrenheitToCelsius(f: Double): Double =
      (f - 32) * 5.0 / 9.0

    def ReadLine: IO[String] = IO {
      readLine
    }

    def PrintLine(msg: String): IO[Unit] = IO {
      println(msg)
    }

    for {
      _ <- PrintLine("Enter a temperature in degrees fahrenheit: ")
      d <- ReadLine.map(_.toDouble)
      _ <- PrintLine(fahrenheitToCelsius(d).toString)
    } yield ()
  }

  def main(args: Array[String]): Unit = {
    test().run
  }

}

object Ch13_Attempt2 {

  trait IO[F[_], +A]

  trait Run[F[_]] {
    def apply[A](expr: F[A]): (A, Run[F])
  }

  case class Pure[F[_], +A](get: A) extends IO[F, A]

  case class Request[F[_], I, +A](expr: F[I], receive: I => IO[F, A]) extends IO[F, A]

  object IO {
    @tailrec
    def run[F[_], A](R: Run[F])(io: IO[F, A]): A = io match {
      case Pure(a) => a
      case Request(expr, recv) =>
        R(expr) match {
          case (e, r2) => run(r2)(recv(e))
        }
    }
  }

  trait Console[A]

  case object ReadLine extends Console[Option[String]]

  case class PrintLine(s: String) extends Console[Unit]

  type ConsoleIO[A] = IO[Console, A]

  def ioMonad[F[_]] = new Monad[({type f[a] = IO[F, a]})#f] {
    override def unit[A](a: => A): IO[F, A] = Pure(a)

    override def flatMap[A, B](ma: IO[F, A])(f: A => IO[F, B]): IO[F, B] = ma match {
      case Pure(a) => f(a)
      case _ => ???
    }
  }


  trait Runnable[A] {
    def run: A
  }

  object Delay {
    def apply[A](a: => A) = new Runnable[A] {
      def run = a
    }
  }


  def main(args: Array[String]): Unit = {

  }


}

object Ch13_Attempt3 {

//  trait Runnable[A] {
//    def run: A
//  }
//
//  object Delay {
//    def apply[A](a: => A) = new Runnable[A] {
//      def run = a
//    }
//  }
//
//
//  trait Trampoline[+A]
//
//  object Trampoline {
//    def run[A](t: Trampoline[A]): A = t match {
//      case Done(value) => value
//      case More(force) => run(force())
//      case Bind(force, f) => force match {
//        case Done(value1: A) => run(f(value1))
//        case More(force1) => run(force1())
//      }
//    }
//  }
//
//  case class Done[+A](get: A) extends Trampoline[A]
//
//  case class More[+A](force: () => Trampoline[A]) extends Trampoline[A]
//
//  case class Bind[A, +B](force: () => Trampoline[A], f: A => Trampoline[B]) extends Trampoline[B]

  def main(args: Array[String]): Unit = {

  }

}

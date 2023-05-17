package code.playground.fpscala

import scala.io.StdIn.readLine

object Ch13 {

  import Ch11.Monad

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
      (f - 32) * 5.0/9.0

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

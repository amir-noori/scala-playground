package code.playground.cats.tutorial

import cats.effect.IO
import cats.effect.unsafe.implicits.global

import scala.annotation.tailrec

object EffectsIO {
  
  val decimalIO: IO[Int] = IO.pure(10)
  val decimalIO2: IO[Int] = IO.pure(20)
  val decimalLazyIO: IO[Int] = IO.delay(10)
  val anotherDecimalLazyIO: IO[Int] = IO { // apply is the same as delay
    10
  }

  val sequencedIOTypes = for {
    i1 <- decimalIO
    i2 <- decimalLazyIO
  } yield i1 + i2

  def sequenceValues(): Unit = {
    val a: IO[Int] = decimalIO.flatMap(_ => decimalIO2)
    val b1: IO[Int] = decimalIO *> decimalIO2 // the same as flatmap and the result is decimalIO2
    val b2: IO[Int] = decimalIO <* decimalIO2 // the same as flatmap and the result is decimalIO
    val c: IO[Int] = decimalIO >> decimalIO2 // the same as flatmap but lazily
  }


  @tailrec
  def forever[A](io: IO[A]): IO[A] = {
    io.unsafeRunSync()
    forever(io)
  }

  // this is tail recursive
  def forever_v1[A](io: IO[A]): IO[A] =
    io >> forever_v1(io)

  // this will result in stack overflow
  def forever_v2[A](io: IO[A]): IO[A] =
    io *> forever_v2(io)

  def forever_[A](io: IO[A]): IO[A] = io.flatMap(a => forever_(IO.pure(a)))

//  def sum(n: Int): IO[Int] = IO(n).flatMap(value => value + sum(value - 1))

//  def sum_(n: Int): IO[Int] = {
//
//    @tailrec
//    def sumTailrec(m: Int, acc: IO[Int]): IO[Int] = {
//      if (m == 0) return acc
//      sumTailrec(m - 1, IO(m).flatMap(i => i + IO(i - 1)))
//    }
//
//    sumTailrec(n, IO(0))
//  }

  def fibonacci(n: Int): IO[BigInt] =
    if (n <= 2) return IO(1)
    else for {
      k <- fibonacci(n - 2)
      m <- fibonacci(n - 1)
    } yield k + m

  def main(args: Array[String]): Unit = {
    (1 to 100).foreach(i => println(i, " -> ", fibonacci(i).unsafeRunSync()))
  }

}

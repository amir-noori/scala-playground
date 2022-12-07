package code.playground.fpscala

import scala.annotation.tailrec

object Ch05 {
  //
  //  object Attempt1 {
  //    sealed abstract class Stream[+A] {
  //      def ::[B >: A](head: B) = ???
  //
  //      def uncons: Option[Cons[A]]
  //
  //      def isEmpty: Boolean = uncons.isEmpty
  //
  //      def toList: List[A] = {
  //
  //        @tailrec
  //        def go(s: Stream[A], acc: List[A]): List[A] = s.uncons match {
  //          case None => acc
  //          case Some(c) => go(c.tail, c.head :: acc)
  //        }
  //
  //        go(this, List()).reverse
  //      }
  //
  //      def take(n: Int): Stream[A] = {
  //        def go(i: Int, s: Stream[A], acc: Stream[A]): Stream[A] = {
  //          if (i == 0) return acc
  //          acc.uncons match {
  //            case None => acc
  //            case Some(c) => go(i - 1, c.tail, c.head :: acc)
  //          }
  //        }
  //
  //        go(n, this, Stream())
  //      }
  //
  //    }
  //
  //    object Empty extends Stream[Nothing] {
  //      val uncons: Option[Nothing] = None
  //
  //    }
  //
  //    sealed abstract class Cons[+A] extends Stream[A] {
  //      def head: A
  //
  //      def tail: Stream[A]
  //
  //      val uncons: Option[Cons[A]] = Some(this)
  //
  //    }
  //
  //    object Stream {
  //      def empty[A]: Stream[A] = Empty
  //
  //      def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = new Cons[A] {
  //        lazy val head: A = hd
  //        lazy val tail: Stream[A] = tl
  //      }
  //
  //      def apply[A](as: A*): Stream[A] =
  //        if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))
  //    }
  //
  //
  //  }
  //

  object Attempt2 {

    import Stream._

    trait Stream[+A] {
      def toList: List[A] = {

        @tailrec
        def go(s: Stream[A], acc: List[A]): List[A] = s match {
          case Empty => acc
          case Cons(h, t) => go(t(), h() :: acc)
        }

        go(this, List()).reverse
      }

      def take(n: Int): Stream[A] = this match {
        case Empty => Empty
        case Cons(h, t) =>
          if (n > 0) cons(h(), t().take(n - 1))
          else empty
      }

      def takeWhile(p: A => Boolean): Stream[A] = this match {
        case Empty => Empty
        case Cons(h, t) =>
          val head = h()
          if (!p(head)) cons(h(), t().takeWhile(p))
          else cons(h(), empty)
      }

      def exists(p: A => Boolean): Boolean = this match {
        case Empty => false
        case Cons(h, t) => if (p(h())) true else t().exists(p)
      }

      /*
        the previous functions (take, takeWhile, exists) follow the same pattern
        which suggests there must be a primitive operation we are missing.
      */

      def takeWhile_(p: A => Boolean): Stream[A] = foldRight(Stream[A]())((c, z) => {
        if (!p(c)) cons(c, z)
        else empty
      })

      // better version of exists which is based on more primitive operation
      def exists_(p: A => Boolean): Boolean = foldRight(false)((c, z) => if (p(c)) true else z)

      def follAll(p: A => Boolean): Boolean = foldRight(true)((c, z) => if (!p(c)) false else z)

      def foldRight[B](z: B)(f: (A, => B) => B): B = this match {
        case Empty => z
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
      }

    }

    case object Empty extends Stream[Nothing]

    case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

    object Stream {
      def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)
      }

      def empty[A]: Stream[A] = Empty

      def apply[A](as: A*): Stream[A] =
        if (as.isEmpty) empty
        else cons(as.head, apply(as.tail: _*))

      val ones: Stream[Int] = Stream.cons(1, ones)

      def from(n: Int): Stream[Int] = ???

      def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
    }

  }

  def main(args: Array[String]): Unit = {
    import code.playground.fpscala.Ch05.Attempt2.Stream

    println("start")
    val cMajor: Stream[String] = Stream[String]("A", "B", "C", "D", "E", "F", "G")
    val t: Stream[String] = cMajor.take(3)
    println(s"List is: ${t.toList}")

    val t2: Stream[String] = cMajor.takeWhile_(a => a == "E")
    println(s"List2 is: ${t2.toList}")

    println("done")
  }

}

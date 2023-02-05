package code.playground.fpscala

import code.playground.cats.effect.FPMonad.Monad

import scala.annotation.tailrec

object Ch11 {


  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]

    def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
      (map(fab)(_._1), map(fab)(_._2))
  }

  trait Monad[M[_]] extends Functor[M] {
    def unit[A](a: => A): M[A]

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(a => unit(f(a)))

    def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
      flatMap(ma)(a => map(mb)(b => f(a, b)))

    def sequence[A](lma: List[M[A]]): M[List[A]] =
      lma.foldLeft(unit(List[A]())) { (acc, ma) =>
        map2(acc, ma)((x, y) => y :: x)
      }

    def sequence_[A](lma: List[M[A]]): M[List[A]] = {
      @tailrec
      def sequenceTaiRec(list: List[M[A]], acc: M[List[A]]): M[List[A]] = list match {
        case x :: xs => sequenceTaiRec(xs, map2(acc, x)((a, b) => b :: a))
        case Nil => acc
      }

      sequenceTaiRec(lma, unit(List[A]()))
    }

    def traverse[A](la: List[A])(f: A => M[A]): M[List[A]] =
      sequence(la.map(a => f(a)))

    def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
      sequence((0 to n).toList.map(_ => ma))

    def factor[A, B](ma: M[A], mb: M[B]): M[(A, B)] =
      map2(ma, mb)((a, b) => (a, b))

    def cofactor[A, B, C[_, _]](mc: C[M[A], M[B]]): M[C[A, B]] = ???

    def cofactor[A, B](e: Either[M[A], M[B]]): M[Either[A, B]] =
      e match {
        case Right(mb) => map(mb)(Right(_))
        case Left(ma) => map(ma)(Left(_))
      }

  }


  val listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = mapTailRec(fa, List())(f)

    @tailrec
    def mapTailRec[A, B](fa: List[A], result: List[B])(f: A => B): List[B] =
      fa match {
        case Nil => result
        case x :: xs => mapTailRec(xs, f(x) :: result)(f)
      }
  }

  val listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] =
      ma match {
        case x :: xs => f(x) ++ flatMap(xs)(f)
        case Nil => Nil
      }
  }

  def main(args: Array[String]): Unit = {
  }

}

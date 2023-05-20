package code.playground.fpscala


import scala.annotation.tailrec

object Ch11 {


  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]

    def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
      (map(fab)(_._1), map(fab)(_._2))
  }

  trait Monad[M[_]] extends Functor[M] {

    def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
      sequence(la.map(a => f(a)))

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

    def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
      sequence((0 to n).toList.map(_ => ma))

    def factor[A, B](ma: M[A], mb: M[B]): M[(A, B)] =
      map2(ma, mb)((a, b) => (a, b))

    def factor[A, B, C](ma: M[A], mb: M[B], mc: M[C]): M[(A, B, C)] =
      map2(map2(ma, mb)((a, b) => (a, b)), mc)((ab, c) => (ab._1, ab._2, c))


    def cofactor[A, B, C[_, _]](mc: C[M[A], M[B]]): M[C[A, B]] = ???

    def cofactor[A, B](e: Either[M[A], M[B]]): M[Either[A, B]] =
      e match {
        case Right(mb) => map(mb)(Right(_))
        case Left(ma) => map(ma)(Left(_))
      }

    def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
      a => flatMap(f(a))(g)

    def flatMap_usingCompose[A, B](ma: M[A])(f: A => M[B]): M[B] =
      compose[Unit, A, B](_ => ma, f)(())

    def idLaw_usingCompose[A](f: A => M[A]) = ???
    //      compose(f, unit[A]) == f && compose(unit[A], f) == f

    def idLaw_usingFlatMap[A](f: A => M[A]) = ???
    //      def f1(a: A): M[A] = flatMap(unit(a))(f)
    //      def f2(a: A): M[A] = flatMap(f(a))(unit)
    //      f1 == f2


    def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(ma => ma)


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

  val optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma match {
      case Some(value) => f(value)
      case _ => None
    }
  }

  // the identity monad
  case class Id[A](value: A) {
    def unit[T](a: T): Id[T] = Id(a)

    def flatMap[B](f: A => Id[B]): Id[B] = f(value)

    def map[B](f: A => B): Id[B] = unit(f(value))
  }

  val idMonad = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)
  }

  case class State[S, A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] =
      State(s => {
        val (a, s1) = run(s)
        (f(a), s1)
      })

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State(s => {
        val (a, s1) = run(s)
        f(a).run(s1)
      })
  }

  // cause state takes two parameters then we have to define a new type to be able to support monad for that type
  type StringState[A] = State[String, A]

  val stringStateMonad = new Monad[StringState] {
    override def unit[A](a: => A): StringState[A] = new StringState(s => (a, s))

    override def flatMap[A, B](ma: StringState[A])(f: A => StringState[B]): StringState[B] =
      ma.flatMap(f)

    def flatMap_[A, B](ma: StringState[A])(f: A => StringState[B]): StringState[B] = ma match {
      case State(run) => new StringState(f compose run)
    }

  }


  def main(args: Array[String]): Unit = {

    val c = for {
      a <- Id("Monads Are")
      b <- Id("Cool")
    } yield s"$a $b"

    println(s"$c")

  }

}

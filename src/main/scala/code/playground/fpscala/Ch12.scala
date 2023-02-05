package code.playground.fpscala


object Ch12 {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]

    def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
      (map(fab)(_._1), map(fab)(_._2))
  }

  trait Applicative[F[_]] extends Functor[F] {
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      apply(apply(unit(f.curried))(fa))(fb)

    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

    def unit[A](a: A): F[A]

    override def map[A, B](fa: F[A])(f: A => B): F[B] =
      apply(unit(f))(fa)
  }

  trait Monad[M[_]] extends Applicative[M] {
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))

    def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(ma => ma)

    def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
      a => flatMap(f(a))(g)

    override def apply[A, B](mf: M[A => B])(ma: M[A]): M[B] =
      flatMap(mf)(f => map(ma)(f))
  }

}

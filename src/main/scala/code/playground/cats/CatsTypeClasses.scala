package code.playground.cats

object CatsTypeClasses {


  trait SemiGroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoid[A] extends SemiGroup[A] {
    def empty: A
  }

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  trait Semigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  trait Applicative[F[_]] extends Apply[F] {
    def pure[A](a: A): F[A]

    override def map[A, B](fa: F[A])(f: A => B): F[B] =
      ap(pure(f), fa)
  }

  trait FlatMap[F[_]] extends Functor[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }

  trait Monad[F[_]] extends Applicative[F] with FlatMap[F] {
    override def map[A, B](fa: F[A])(f: A => B): F[B] =
      flatMap(fa)(a => pure(f(a)))
  }



  trait Apply[F[_]] extends Semigroupal[F] with Functor[F] {
    def ap[A, B](fab: F[A => B], fa: F[A]): F[B]

    override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
      val f: A => B => (A, B) = (a: A) => (b: B) => (a, b)
      ap(map(fa)(f), fb)
    }

    def mapN[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      map(product(fa, fb)) {
        case (a, b) => f(a, b)
      }
  }


  def temTimes[F[_]](x: F[Int])(implicit functor: Functor[F]): F[Int] =
    functor.map(x)(a => a * 10)


}

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


object TypeClasses {

  trait Monoid[A] {
    def empty: A

    def combine(x: A, y: A): A
  }

  implicit val intAdditionMonoid: Monoid[Int] = new Monoid[Int] {
    def empty: Int = 0

    def combine(x: Int, y: Int): Int = x + y
  }

  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    def empty: String = ""

    def combine(x: String, y: String): String = x + y
  }

  case class SetMonoid[A]() extends Monoid[Set[A]] {
    override def empty: Set[A] = Set[A]()

    override def combine(x: Set[A], y: Set[A]): Set[A] = x union y
  }

  object SetMonoid {
    implicit val intSetMonoid: Monoid[Set[Int]] = SetMonoid[Int]()
    implicit val stringSetMonoid: Monoid[Set[String]] = SetMonoid[String]()
  }


  def combineAll[A](list: List[A])(implicit m: Monoid[A]): A = list.foldRight(m.empty)(m.combine)


  def sumInts_(list: List[Int]): Int =
    list.foldRight(intAdditionMonoid.empty)(intAdditionMonoid.combine)

  def concatStrings_(list: List[String]): String =
    list.foldRight(stringMonoid.empty)(stringMonoid.combine)

  def unionSets_[A](list: List[Set[A]]): Set[A] =
    list.foldRight(SetMonoid[A]().empty)(SetMonoid[A]().combine)


  import SetMonoid._

  def sumInts(list: List[Int]): Int = combineAll(list)

  def concatStrings(list: List[String]): String = combineAll(list)

  def unionSets[A](list: List[Set[A]])(implicit m: Monoid[Set[A]]): Set[A] = combineAll(list)

  def main(args: Array[String]): Unit = {
    sumInts(List(10, 20, 30))
    concatStrings(List("a", "b", "c"))
    unionSets(List(Set(10), Set(20), Set(30)))
  }


  case class Pair[A, B](a: A, b: B)

  def pairMonoid[A, B](implicit ma: Monoid[A], mb: Monoid[B]) = new Monoid[Pair[A, B]] {
    override def empty: Pair[A, B] = Pair(ma.empty, mb.empty)

    override def combine(x: Pair[A, B], y: Pair[A, B]): Pair[A, B] =
      Pair(ma.combine(x.a, y.a), mb.combine(x.b, y.b))
  }

  implicit val intStringPairMonoid: Monoid[Pair[Int, String]] = pairMonoid[Int, String]

  def mergePairs(list: List[Pair[Int, String]])(implicit m: Monoid[Pair[Int, String]]): Pair[Int, String] =
    combineAll(list)(m)

  mergePairs(List(Pair(10, "a"), Pair(20, "b")))


}

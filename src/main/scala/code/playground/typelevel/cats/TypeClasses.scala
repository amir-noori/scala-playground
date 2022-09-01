package code.playground.typelevel.cats

object TypeClasses extends App {

  import cats.Monoid

  final case class Pair[A, B](first: A, second: B)

  object Pair {
    implicit def derivedMonoidPair[A, B](implicit ma: Monoid[A], mb: Monoid[B]): Monoid[Pair[A, B]] = {
      new Monoid[Pair[A, B]] {
        override def empty: Pair[A, B] =
          Pair(ma.empty, mb.empty)

        override def combine(x: Pair[A, B], y: Pair[A, B]): Pair[A, B] = {
          Pair(
            ma.combine(x.first, y.first),
            mb.combine(x.second, y.second)
          )
        }
      }
    }

  }


  def combineAll[A](list: List[A])(implicit A: Monoid[A]) =
    list.foldRight(A.empty)(A.combine)


  val pairs: List[Pair[String, Int]] = List(Pair("Jack", 10), Pair("Jim", 20), Pair("Joe", 30))
  combineAll(pairs)

}

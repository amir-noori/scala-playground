package code.playground.cats

object Types {

  trait Monoid[A] {
    def empty: A

    def combine(x: A, y: A): A
  }


  def deriveMonoidPair[A, B](A: Monoid[A], B: Monoid[B]): Monoid[Pair[A, B]] =
    new Monoid[Pair[A, B]] {
      def empty: Pair[A, B] = Pair(A.empty, B.empty)

      def combine(x: Pair[A, B], y: Pair[A, B]): Pair[A, B] =
        Pair(A.combine(x.first, y.first), B.combine(x.second, y.second))
    }

  final case class Pair[A, B](first: A, second: B)

  object Pair {
    implicit def tuple2Instance[A, B](implicit A: Monoid[A], B: Monoid[B]): Monoid[Pair[A, B]] =
      new Monoid[Pair[A, B]] {
        def empty: Pair[A, B] = Pair(A.empty, B.empty)

        def combine(x: Pair[A, B], y: Pair[A, B]): Pair[A, B] =
          Pair(A.combine(x.first, y.first), B.combine(x.second, y.second))
      }
  }

}

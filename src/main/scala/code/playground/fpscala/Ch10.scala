package code.playground.fpscala

import scala.language.implicitConversions

object Ch10 {


  object Attempt1 {

    trait Monoid[A] { self =>
      def op(a1: A, a2: A): A

      def id(): A
    }

    val stringMonoid: Monoid[String] = new Monoid[String] {
      override def op(a1: String, a2: String): String = a1 + a2

      override def id(): String = ""
    }

    def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
      override def op(a1: List[A], a2: List[A]): List[A] = a1 ::: a2

      override def id(): List[A] = List[A]()
    }

    def optionMonoid[A >: Monoid[_]](implicit mA: Monoid[A]): Monoid[Option[A]] = new Monoid[Option[A]] {

      override def op(a1: Option[A], a2: Option[A]): Option[A] =
        for {
          value1 <- a1
          value2 <- a2
        } yield implicitly[Monoid[A]].op(value1, value2)

      override def id(): Option[A] = None
    }

    // a monoid for endo functions
    def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
      override def op(f1: A => A, f2: A => A): A => A = f1.compose(f2)

      override def id(): A => A = (a: A) => a
    }

    def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
      def op(x: A, y: A): A = m.op(y, x)

      def id(): A = m.id()
    }

    def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
      as.foldLeft(m.id())((b, a) => m.op(b, f(a)))

    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
      /*
        f: (A, B) => B is f: A => B => B is f: A => (B => B)
        now consider (B => B) is X then f: A => X then
        foldMap(as, ?)(f: A => X) is:
        foldMap(as, Monoid[X])(f: A => X): X is:
        foldMap(as, Monoid[B => B])(f: A => (B => B)): X is:
        foldMap(as, Monoid[B => B])(f: A => (B => B)): X is:
        foldMap(as, endoMonoid[B])(f.curried): X is:
        foldMap(as, endoMonoid[B])(f.curried): (B => B)
       */
      foldMap(as, endoMonoid[B])(a => b => f(a, b))(z)
    }

    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

    def concatenate[A](as: List[A], m: Monoid[A]): A =
      foldMap(as, m)(a => a)

    def concatString(as: List[String]): String =
      foldMap(as, stringMonoid)(a => a)

    def concatenate_[A](as: List[A], m: Monoid[A]): A = {
      as match {
        case x :: xs => m.op(x, concatenate_(xs, m))
        case Nil => m.id()
      }
    }


    object MonoidTest {

      val words = List("Monoids ", "Are ", "Cool")
      val sentence1: String = words.foldRight(stringMonoid.id())(stringMonoid.op)
      val sentence2: String = words.foldLeft(stringMonoid.id())(stringMonoid.op)
      val sentence3: String = words.fold(stringMonoid.id())(stringMonoid.op)
      println(sentence1)
      println(sentence2)
      println(sentence3)

    }


    def main(args: Array[String]) = {
      println("attempt 1")

    }

  }


  def main(args: Array[String]) =
    Attempt1.main(args)

}

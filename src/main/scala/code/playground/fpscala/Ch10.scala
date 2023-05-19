package code.playground.fpscala

import scala.language.implicitConversions

object Ch10 {


  object Attempt1 {

    trait Monoid[A] {
      self =>
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

    def foldMap[A, B](i: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
      if (i.isEmpty) m.id()
      else i.splitAt(i.length / 2) match {
        case (l, r) if r.isEmpty => f(l(0))
        case (l, r) if l.isEmpty => f(r(0))
        case (l, r) => m.op(foldMap(l, m)(f), foldMap(r, m)(f))
      }
    }

    def ordered(ints: IndexedSeq[Int]): Boolean = {
      // Our monoid tracks the minimum and maximum element seen so far
      // as well as whether the elements are so far ordered.
      val mon = new Monoid[Option[(Int, Int, Boolean)]] {
        def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]) =
          (o1, o2) match {
            // The ranges should not overlap if the sequence is ordered.
            case (Some((x1, y1, p)), Some((x2, y2, q))) =>
              println(s"x1($x1), y1($y1), x2($x2), y2($y2)")
              Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
            case (x, None) => x
            case (None, x) => x
          }

        def id() = None
      }
      // The empty sequence is ordered, and each element by itself is ordered.
      foldMap(ints, mon)(i => Some((i, i, true))).map(_._3).getOrElse(true)
    }


    sealed trait WC

    case class Stub(chars: String) extends WC

    case class Part(lStub: String, words: Int, rStub: String) extends WC

    val wcMonoid: Monoid[WC] = new Monoid[WC] {
      def op(a: WC, b: WC) = (a, b) match {
        case (Stub(c), Stub(d)) => Stub(c + d)
        case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
        case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
        case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
          Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
      }

      override def id(): WC = Stub("")
    }

    def count(s: String): Int = {
      def wc(c: Char): WC =
        if (c.isWhitespace)
          Part("", 0, "")
        else
          Stub(c.toString)

      def unstub(s: String) = s.length min 1

      foldMap(s.toIndexedSeq, wcMonoid)(wc) match {
        case Stub(s) => unstub(s)
        case Part(l, w, r) => unstub(l) + w + unstub(r)
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

    trait Foldable[F[_]] {
      def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
        foldMap(as)(f.curried)(endoMonoid[B])(z)

      def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
        foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

      def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
        // foldRight(as)(mb.id())((a, b) => mb.op(f(a), b))

      def concatenate[A](as: F[A])(m: Monoid[A]): A =
        foldLeft(as)(m.id())(m.op)

      def toList[A](fa: F[A]): List[A] =
        foldLeft(fa)(List[A]())((acc, a) => a :: acc)
    }

    sealed trait Tree[+A]
    case class Leaf[A](value: A) extends Tree[A]
    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

    case class FoldableTree() extends Foldable[Tree] {
      override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
          case Leaf(a) => f(a)
          case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
        }
    }




    def main(args: Array[String]) = {
      println("attempt 1")
      //      println("ordered: " + ordered(IndexedSeq(10, 20, 30, 40, 50, 60, 70)))

      println(count("this is a test string, have fun!    "))

    }

  }


  def main(args: Array[String]) =
    Attempt1.main(args)

}

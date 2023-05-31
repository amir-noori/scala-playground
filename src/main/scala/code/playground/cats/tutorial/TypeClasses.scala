package code.playground.cats.tutorial

import scala.language.implicitConversions

object TypeClasses {

  object FunctorTypes {

    import cats.Functor
    import cats.instances.list._

    def incrementIntByFunctor[F[_]](z: F[Int])(implicit functor: Functor[F]): F[Int] =
      functor.map(z)(a => a + 1)

    trait Induction[A] {
      def next(): A
    }

    case class NaturalNumber(value: Int) extends Induction[NaturalNumber] {
      override def next(): NaturalNumber = NaturalNumber(value + 1)
    }

    implicit def toNatural(i: Int): NaturalNumber = NaturalNumber(i)


    def incrementByFunctor[F[_], A <: Induction[A]](z: F[A])(implicit functor: Functor[F]): F[A] =
      functor.map(z)(a => a.next())

    def test(): Unit = incrementByFunctor(Option(NaturalNumber(10))) match {
      case Some(result) => println(s"integer incremented by functor: ${result.value}")
      case None => println(s"next value is not there!")
    }
  }

  object ApplicativeTypes {
    // applicative is used to wrap types into other types

    import cats.Applicative
    import cats.instances.list._
    import cats.syntax.applicative._

    val l: List[Int] = 10.pure[List]
    l.foreach(i => println(s"i: $i"))

  }

  object TraverseTypes {

    def test(): Unit = {
      import cats.syntax.all._

      val list: List[Option[Int]] = List(Some(1), Some(2), Some(3))

      val traversed: Option[List[Int]] = list.traverse(identity)
      traversed match {
        case Some(value) => value.foreach(i => println(s"i: $i"))
        case None => println("!!!")
      }
    }

  }


  def main(args: Array[String]): Unit = {

  }

}

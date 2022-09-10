package code.playground.fpscala

import scala.language.implicitConversions

object Ch09 extends App {


  object Attempt1 {

    trait Parsers[ParseError, Parser[+_]] { self =>
      def char(c: Char): Parser[Char] = string(c.toString) map ((x: String) => x.charAt(0))

      def succeed[A](a: A): Parser[A] = string("") map ((_: String) => a)

      def string(s: String): Parser[String]

      def orString(s1: String, s2: String): Parser[String]

      def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

      def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

      implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

      implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

      def run[A](p: Parser[A])(input: String): Either[ParseError, A]

      def product[A, B](p: Parser[A], p2: Parser[B]): Parser[(A, B)]

      def map2[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] =
        product(p, p2).map[(A, B), C](f.tupled)

      def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

      def many[A](p: Parser[A]): Parser[List[A]]

      case class ParserOps[A](p: Parser[A]) {
        def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

        def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

        def map[A, B](f: A => B): Parser[B] = ???

        def many[A](p: Parser[A]): Parser[List[A]] = ???

        def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

        def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
      }

    }


    object Test1 {

      case class MyParser[+A]() {}

      case class MyParsers() extends Parsers[String, MyParser] {
        override def char(c: Char): MyParser[Char] = ???

        override def string(s: String): MyParser[String] = ???

        override def orString(s1: String, s2: String): MyParser[String] = ???

        override def or[A](s1: MyParser[A], s2: MyParser[A]): MyParser[A] = ???

        override def run[A](p: MyParser[A])(input: String): Either[String, A] = ???

        override def listOfN[A](n: Int, p: MyParser[A]): MyParser[List[A]] = ???

        override def product[A, B](p: MyParser[A], p2: MyParser[B]): MyParser[(A, B)] = ???

        override def many[A](p: MyParser[A]): MyParser[List[A]] = ???
      }

      val P: MyParsers = ???

      import P._


      implicit def f(a: String): MyParser[String] = ???

      "asdasd" | "aasdasd" | "fgdfg"


    }

  }


  //  object Attempt2 {
  //
  //    trait Result[R, E]
  //
  //    trait AbstractParser[INPUT, RESULT[_, _]] {
  //      def parse(input: INPUT): RESULT
  //    }
  //
  //    trait ParserApplier[Parser[_, _]] {
  //      def run[I, R[_, _]](input: I)(p: Parser[I, R[_, _]]): R[_, _]
  //    }
  //
  //  }
  //
  //
  //  object Attempt3 {
  //
  //    trait Parser[A] {}
  //
  //
  //    def many[A](p: Parser[A]): Parser[List[A]] = ???
  //
  //    def map[A, B](a: Parser[A])(f: A => B): Parser[B] = ???
  //
  //  }


}

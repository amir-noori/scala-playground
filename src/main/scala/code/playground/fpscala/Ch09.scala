package code.playground.fpscala

import scala.language.implicitConversions

object Ch09 {


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

  object Attempt2 {

    trait Result[R, E]

    trait AbstractParser[INPUT, RESULT[_, _]] {
      //      def parse(input: INPUT): RESULT
    }

    trait ParserApplier[Parser[_, _]] {
      def run[I, R[_, _]](input: I)(p: Parser[I, R[_, _]]): R[_, _]
    }

  }

  object Attempt3 {

    trait Parser[A] {
    }

    trait Parsers[ParseError, Parser[+_]] { self =>
      def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???

      def char(c: Char): Parser[Char] = ???

      def or[A](p1: Parser[A], p2: Parser[A]): Parser[A] = ???

      implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

      implicit def string(s: String): Parser[String] = ???

      implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

      implicit def asParser[A, B](a: A)(implicit f: A => Parser[B]): ParserOps[B] = ParserOps[B](f(a))


      case class ParserOps[A](p: Parser[A]) {
        def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

        def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
      }

    }


  }

  object Attempt4 {


    sealed trait Parser[A] { self =>
      def unit[B](a: B): Parser[B]

      def char(c: Char): Parser[A]

      def combine(p: Parser[_]): Parser[A]

      def unit[B](a: B): Parser[B]

      def map[B](f: A => B): Parser[B] = self.flatMap(a => unit(f(a)))

      def flatMap[B](f: A => Parser[B]): Parser[B]

    }

    //    case class JsonParser() extends Parser[String] { self =>
    //      override def char(c: Char): Parser[String] = ???
    //
    //      override def combine(p: Parser[_]): Parser[String] = ???
    //
    //      override def unit[B >: String](a: B): Parser[B] = ???
    //
    //      override def flatMap[B >: String](f: String => Parser[B]): Parser[B] = ???
    //    }

    case class CharACounterParser(count: Int = 0) extends Parser[Int] { self =>
      override def char(c: Char): Parser[Int] =
        if (c == 'a') CharACounterParser(count + 1) else CharACounterParser(count)

      override def combine(p: Parser[_]): Parser[Int] =
        p match {
          case CharACounterParser(value) => CharACounterParser(count + value)
          case _ => CharACounterParser(count)
        }

      override def toString: String = s"$count"

      override def unit[B](a: B): Parser[B] = ???

      override def flatMap[B](f: Int => Parser[B]): Parser[B] =
        self match {
          case CharACounterParser(value) => f(value)
          case _ => f(0)
        }

    }

    object Parser {

      type ParseError = String

      def or[A](p1: Parser[A], p2: Parser[A]): Parser[A] = p1.combine(p2)

      implicit def string(s: String): Parser[String] = ???

      implicit def operators[A](p: Parser[A]) =
        ParserOps[A](p)

      implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
        ParserOps(f(a))

      def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = ???

      def many[A](p: Parser[A]): Parser[List[A]] =
        map2(p, many(p))(_ :: _) or p.unit(List())

      def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
        for {a <- p; b <- p2} yield f(a, b)


      case class ParserOps[A](p: Parser[A]) {
        def |(p2: Parser[A]): Parser[A] = Parser.or(p, p2)

        def or(p2: => Parser[A]): Parser[A] = Parser.or(p, p2)
      }

      def run[A](p: Parser[A], s: String): Either[ParseError, A] = ???

    }

    object ParserLaws {

      import Parser._

      def id[A](a: A): A = a

      def mapLaw[A](p: Parser[A]) = p.map(id) == p

    }

    def main(args: Array[String]): Unit = {

      //      val jsonParser = JsonParser()
      // the following will automatically change jsonParser to ParserOps[JsonParser] and String into Parser[String] and into ParserOps[String]
      //      jsonParser | ""

      val charParser1 = CharACounterParser()
      val charParser2 = CharACounterParser()
      val a = charParser1
        .char('a')
        .char('b')
        .char('a')
      val b = charParser2
        .char('a')
        .char('a')
        .char('c')

      val c: Parser[Int] = a | b
      println(s"char parser result -> ${a}")
      println(s"char parser result -> ${b}")
      println(s"char parser result -> ${c}")

    }

  }

  object Attempt5 {


    trait ParserCombinator[Parser[+_], ParseError] {

      type ParseError = String

      def string(s: String): Parser[String] = ???

      def succeed[A](a: A): Parser[A] = ???

      def many[A](p: Parser[A]): Parser[List[A]] = ???

      def chars(s: String): Parser[List[Char]] = ???

      def or[A](p1: Parser[A], p2: Parser[A]): Parser[A] = ???

      def run[A](p: Parser[A])(input: String): Either[A, ParseError] = ???

      def char(a: Char): Parser[Char]

      def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = ???

      implicit def ops[A](p: Parser[A]): ParserOps[A] = ParserOps(p)

      def map[A, B](f: A => B): Parser[B] = ???

      def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???

      def fold[A, B](z: B)(f: (A, B) => B): Parser[B] = ???


      /**
       * Syntax class
       *
       * @param p
       * @tparam A
       */
      case class ParserOps[A](p: Parser[A]) { self =>

        def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p2)

        def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p2)

        def map[B](f: A => B): Parser[B] = self.map(f)

        def many[B](p2: Parser[B]): Parser[List[A]] = self.many(p2)

        def flatMap[B >: A](f: A => Parser[B]): Parser[B] = self.flatMap(f)

        // Parser[List[Char]].fold(0)()
        def fold[B >: A](z: B)(f: (A, B) => B): Parser[B] = self.fold(z)(f)

      }

    }

    object ParserCombinator {

    }

    object CharParserTest {

      type ParseError = String

      case class Parsing[Parser[+_]](P: ParserCombinator[Parser, ParseError], c: Char) { self =>

        import P._

        def doRun[A](p: Parser[A], s: String) = run(p)(s)

      }

      case class CharCountParser[+A](c: A, count: Int) { self =>
        def map[B](f: A => B): CharCountParser[B] =
          self match {
            case CharCountParser(ch, n) => CharCountParser(f(ch), n)
          }
      }

      case class CharParsing() extends ParserCombinator[CharCountParser, String] { self =>

        override def map[A, B](f: A => B): CharCountParser[B] = ???

        override def string(s: String): CharCountParser[String] = ???

        override def chars(s: String): CharCountParser[List[Char]] = ???

        override def or[A](p1: CharCountParser[A], p2: CharCountParser[A]): CharCountParser[A] = ???

        override def run[A](p: CharCountParser[A])(input: String): Either[A, String] = ???
        //        {
        //          val parser = chars(input).map(listCh =>
        //            listCh.foldLeft(0)((ch, count) => ch match {
        //              case p.c => count + 1
        //              case _ => count
        //            }))
        //          p match {
        //            case CharCountParser(ch, _) => Right("this actually must be left!")
        //            case _ => Right("err")
        //          }
        //        }

        override def listOfN[A](n: Int, p: CharCountParser[A]): CharCountParser[List[A]] = ???

        override def fold[A, B](z: B)(f: (A, B) => B): CharCountParser[B] = ???

        override def many[A](p: CharCountParser[A]): CharCountParser[List[A]] = ???

        override def succeed[A](a: A): CharCountParser[A] = ???

        override def flatMap[A, B](p: CharCountParser[A])(f: A => CharCountParser[B]): CharCountParser[B] = ???

        override def char(a: Char): CharCountParser[Char] = ???
      }

    }

    object CharParserTest2 {

      case class CharCountParser[+A](input: A) extends ParserCombinator[CharCountParser, String] { self =>
        def map_[B](f: A => B): CharCountParser[B] = self match {
          case CharCountParser(ch) => CharCountParser(f(ch))
        }

        def many_[B >: A](p: CharCountParser[B]): CharCountParser[List[Char]] =
          self match {
            case CharCountParser(s: String) =>
              p match {
                case CharCountParser(ch: Char) => CharCountParser(s.foldLeft(List[Char]())((l, c) => {
                  if (ch == c) ch :: l else l
                }))
              }
            case CharCountParser(s: Char) =>
              if (s == input) CharCountParser(List[Char](s)) else CharCountParser(List[Char]())
          }

        override def string(s: String): CharCountParser[String] =
          CharCountParser(s)

        override def char(a: Char): CharCountParser[Char] =
          string(a.toString) map_ (_.charAt(0))
      }

      case class CharCountParsing(P: ParserCombinator[CharCountParser, String]) {

        import P._

        def howMany(input: String, c: Char): CharCountParser[Int] = {
          string(input)
            .many_(char(c))
            .map_((a: Seq[Char]) => a.size)
        }

      }

    }


    def main(args: Array[String]): Unit = {
      println("attempt 5 ...")

      //      CharParserTest
      //        .CharParsing()
      //        .run(CharParserTest.CharCountParser[Char]('a', 0))("this is a test string. all right.")
      //
      val P = CharParserTest2.CharCountParser[Char]('s')
      val result: CharParserTest2.CharCountParser[Int] = CharParserTest2
        .CharCountParsing(P)
        .howMany("this is just a test.", 's')
      println(s"result -> $result")
    }


  }

  def main(args: Array[String]): Unit = {
    Attempt5.main(args)
  }

}



package code.playground.fp

import scala.io.StdIn.readLine

object FreeMonads extends App {

  trait Monad[M[_]] {
    def pure[A](a: A): M[A]

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  }

  object Monad {
    def apply[M[_]](implicit monad: Monad[M]): Monad[M] = monad
  }

  trait ~>[F[_], G[_]] {
    def apply[A](fa: F[A]): G[A]
  }

  /**
   * Consider Free as a program that will be running
   *
   * @tparam S consider it as a language
   * @tparam A consider it as the ned result
   */
  trait Free[S[_], A] {
    self =>

    import Free._

    def foldMap[T[_] : Monad](transformer: S ~> T): T[A] = self match {
      case Pure(a) => Monad[T].pure(a)
      case Suspend(sa) => transformer(sa)
      case FlatMapped(sa, f) =>
        val ta: T[A] = sa.foldMap(transformer).asInstanceOf[T[A]]
        Monad[T]
          .flatMap(ta) { a: A =>
            f(a).foldMap(transformer)
          }
    }

    def flatMap[B](f: A => Free[S, B]): Free[S, B] = FlatMapped(self, f)

    def map[B](f: A => B): Free[S, B] = flatMap(a => pure(f(a)))

  }

  object Free {
    // to lift any language into a free monad
    def lift[S[_], A](sa: S[A]): Free[S, A] = Suspend(sa)

    def pure[S[_], A](a: A): Free[S, A] = Pure(a)


    case class Pure[S[_], A](a: A) extends Free[S, A]

    case class Suspend[S[_], A](sa: S[A]) extends Free[S, A]

    case class FlatMapped[S[_], A, B](sa: Free[S, A], f: A => Free[S, B]) extends Free[S, B]

  }


  // lets define a language (DSL)
  sealed trait InOut[A]

  case class PrintLine(s: String) extends InOut[Unit]

  case class GetLine() extends InOut[String]

  // now lets lift the DSL into a free monad
  object InOut {
    def printLine(s: String): Free[InOut, Unit] = Free.lift(PrintLine(s))

    def getLine: Free[InOut, String] = Free.lift(GetLine())
  }

  // now we need an interpreter of our DSL

  case class Console[A](run: () => A)

  object Console {
    def start[A](a: => A): Console[A] = Console(() => a)
  }

  implicit def consoleMonad = new Monad[Console] {
    override def pure[A](a: A): Console[A] =
      Console(() => a)

    override def flatMap[A, B](ma: Console[A])(f: A => Console[B]): Console[B] = ma match {
      case Console(a) => f(a())
      case _ => throw new IllegalArgumentException("error with flatMap!")
    }
  }

  // define the transformer
  val inOutToConsoleTransformer: InOut ~> Console = new (InOut ~> Console) {
    override def apply[A](fa: InOut[A]): Console[A] = fa match {
      case PrintLine(s) => Console.start(println(s"-> $s"))
      case GetLine() => Console.start(readLine())
    }
  }

  // the business logic
  val logic: Free[InOut, Unit] = for {
    _ <- InOut.printLine("what is the id?")
    id <- InOut.getLine
    _ <- InOut.printLine(s"id: ${id}")
  } yield ()

  logic.foldMap(inOutToConsoleTransformer).run()

}

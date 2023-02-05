package code.playground.cats.effect

import cats.effect.{IO, IOApp}

import scala.concurrent.duration.DurationInt

object Hello extends IOApp.Simple {
  override def run: IO[Unit] = for {
    _ <- IO.print("hello")
    name = "world"
    _ <- IO.println(s" $name")
  } yield IO.unit
}

object NumPrint extends IOApp.Simple {
  override def run: IO[Unit] = for {
    ref <- IO.ref(0)
    value <- ref.get
    _ <- IO.println(s"value is: $value")
    newRef <- ref.update(a => a + 10)
    _ <- IO.println(s"updated value is: $newRef")
  } yield IO.unit
}

object Loop extends IOApp.Simple {
  override def run: IO[Unit] = {
    lazy val loop: IO[Unit] = IO.println("Hello, World!") >> IO.sleep(1.second) >> loop
    loop.timeout(5.seconds)
  }
}

object AsyncApp extends IOApp.Simple {

  import java.util.concurrent.{Executors, TimeUnit}

  override def run: IO[Unit] = {
    val scheduler = Executors.newScheduledThreadPool(1)

    IO.async_[Unit](cb => {
      scheduler.schedule(new Runnable {
        def run = {
          println("all right!")
          cb(Right(()))
        }
      }, 500, TimeUnit.MILLISECONDS)

      ()
    })

  }
}


object FPMonoid extends App {

  trait Monoid[A] {
    def combine(a: A, b: A): A

    def empty(): A
  }

  implicit val intMonoid = new Monoid[Int] {
    override def combine(a: Int, b: Int): Int = a + b

    override def empty(): Int = 0
  }

  implicit val stringMonoid = new Monoid[String] {
    override def combine(a: String, b: String): String = a + b

    override def empty(): String = ""
  }


  case class Product(name: String, price: Int)

  implicit val productMonoid = new Monoid[Product] {
    override def combine(a: Product, b: Product): Product = a match {
      case Product("", 0) => b
      case _ => Product(s"${a.name}, ${b.name}", a.price + b.price)
    }

    override def empty(): Product = Product("", 0)
  }


  def sum[A](l: List[A])(implicit m: Monoid[A]) = l.fold(m.empty())(m.combine)


  println(sum(List(1, 2, 4, 10)))
  println(sum(List("Hello ", "There")))

  println(sum(List(Product("first", 110), Product("second", 201))))

}


object FPFunctor extends App {

  trait D {
    def *(other: D): D
  }

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  object Functor {
    implicit val listFunctor = new Functor[List] {
      override def map[A, B](fa: List[A])(f: A => B): List[B] = fa match {
        case x :: xs => f(x) :: map(xs)(f)
        case Nil => List()
      }
    }

    implicit val optionFunctor = new Functor[Option] {
      override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
        case Some(value) => Some(f(value))
        case None => None
      }
    }
  }

  def doubleIt[F[_]](list: F[Int])(implicit functor: Functor[F]): F[Int] =
    functor.map(list)(n => n * 2)

  println(doubleIt(List(10, 20, 30)))
  println(doubleIt(Option(10)))

}

object FPMonad extends App {

  import FPFunctor._

  trait Monad[F[_]] extends Functor[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    def pure[A](value: A): F[A]

    def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(x => pure(f(x)))
  }

  object Monad {
    implicit val listMonad = new Monad[List] {
      override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa match {
        case x :: xs => f(x).appendedAll(flatMap(xs)(f))
        case Nil => Nil
      }

      override def pure[A](value: A): List[A] = List(value)
    }

    implicit val optionMonad = new Monad[Option] {
      override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
        case Some(value) => f(value)
        case None => None
      }

      override def pure[A](value: A): Option[A] = Option(value)
    }
  }

}



object A extends App {

  val twoDList = List(List(1,2), List(3,4), List(5,6))

  val y = twoDList.flatMap(el => el :: List(10))
  val y2 = twoDList.map(el => el :: List(10))

  println(y)
  println(y2)
}


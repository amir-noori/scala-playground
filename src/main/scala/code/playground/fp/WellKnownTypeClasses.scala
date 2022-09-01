package code.playground.fp

import cats._
import cats.implicits._

import scala.annotation.tailrec

object WellKnownTypeClasses extends App {

  /////////////////////////////////////////////////////////////////////////////////////

  object ShowApp {
    case class Account(id: Long, number: String, balance: Double, owner: String)

    object Account {

      // implicit val orderById: Order[Account] = Order.from((x, y) => Order[Long].compare(x.id, y.id))
      implicit def orderById(implicit ordering: Order[Long]): Order[Account] =
        Order.from((x, y) => ordering.compare(x.id, y.id))

      implicit val showAccount: Show[Account] = Show.show(account => {
        s"Account: NUMBER: ${account.number} - BALANCE: ${account.balance}"
      })
    }

    val account1 = Account(1, "1", 100, "test1")
    val account2 = Account(2, "2", 200, "test2")
    // we can use compare because an implicit value of Order[Account] is provided
    account1 compare account2

    // we can use compare because an implicit value of Show[Account] is provided
    println(account1.show)
  }


  /////////////////////////////////////////////////////////////////////////////////////

  object MonoidApp {
    case class Speed(metersPerSecond: Double) {
      def kilometersPerSec: Double = metersPerSecond / 1000

      def milesPerSec: Double = metersPerSecond / 1609.34
    }

    object Speed {

      implicit def universalEquality: Eq[Speed] = Eq.fromUniversalEquals[Speed]

      def addSpeeds(s1: Speed, s2: Speed): Speed = {
        Speed(s1.metersPerSecond + s2.metersPerSecond)
      }

      // implicit val speedMonoid: Monoid[Speed] = Monoid.instance(Speed(0), addSpeeds)
      implicit val speedMonoid: Monoid[Speed] = new Monoid[Speed] {
        override def empty: Speed = Speed(0)

        override def combine(x: Speed, y: Speed): Speed = addSpeeds(x, y)
      }

    }

    val speed1: Speed = Speed(100)
    val speed2: Speed = Speed(200)
    val speed3: Speed = Speed(300)

    println(Monoid[Speed].combine(speed1, speed2) === speed3) // === is the syntax for the implicit universalEquality
    println(speed1 |+| speed2) // |+| is the syntax for Monoid[Speed].combine


    val sumMonoid: Monoid[Int] = new Monoid[Int] {
      override def empty: Int = 0

      override def combine(x: Int, y: Int): Int = x + y
    }

    val minMonoid: Monoid[Int] = new Monoid[Int] {
      override def empty: Int = Int.MaxValue

      override def combine(x: Int, y: Int): Int = if (x < y) x else y
    }

    val stringMonoid: Monoid[String] = new Monoid[String] {
      override def empty: String = ""

      override def combine(x: String, y: String): String = x ++ y
    }


    def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
      override def empty: List[A] = Nil

      override def combine(x: List[A], y: List[A]): List[A] = x ++ y
    }
  }

  /////////////////////////////////////////////////////////////////////////////////////


  def parseInt(str: String): Option[Int] = scala.util.Try(str.toInt).toOption

  def divide(a: Int, b: Int): Option[Int] = if (b == 0) None else Some(a / b)

  def stringDivideBy(str1: String, str2: String): Option[Int] =
    parseInt(str1).flatMap(x =>
      parseInt(str2).flatMap(y =>
        divide(x, y)
      )
    )

  def stringDivideByEnhanced(str1: String, str2: String): Option[Int] =
    for {
      x <- parseInt(str1)
      y <- parseInt(str2)
      r <- divide(x, y)
    } yield r

  stringDivideBy("10", "5")
  stringDivideByEnhanced("10", "5")



  /////////////////////////////////////////////////////////////////////////////////////


  object ApplicativeApp {
    //  sealed trait Validated[+A]
    //
    //  case class Valid[A](a: A) extends Validated[A]
    //
    //  case class Invalid[A](errors: List[String]) extends Validated[Nothing]
    //
    //  def map2[A, B, C](va: Validated[A], vb: Validated[B])(f: (A, B) => C): Validated[C] = {
    //    (va, vb) match {
    //      case (Valid(a), Valid(b)) => Valid(f(a, b))
    //      case (Invalid(e), Valid(_)) => Invalid(e)
    //      case (Valid(_), Invalid(e)) => Invalid(e)
    //      case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2)
    //    }
    //  }
    //
    //  case class Person(name: String, age: Int) {
    //    def validateName(): Validated[String] = {
    //      if (name.forall(_.isLetter)) Valid(name)
    //      else Invalid[String](List("The name should only contain letters"))
    //    }
    //
    //    def validateAge(): Validated[Int] = {
    //      if (age >= 18) Valid(age)
    //      else Invalid[Int](List("age is less than 18"))
    //    }
    //
    //    def validatePerson(): Validated[Person] = {
    //      (validateName(), validateAge()) match {
    //        case (Valid(_), Valid(_)) => Valid(this)
    //        case (Invalid(e), Valid(_)) => Invalid(e)
    //        case (Valid(_), Invalid(e)) => Invalid(e)
    //        case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2)
    //      }
    //    }
    //  }

    sealed trait Validated[+A]

    object Validated {
      case class Valid[+A](a: A) extends Validated[A]

      case class Invalid(errors: List[String]) extends Validated[Nothing]

      implicit val applicative: Applicative[Validated] = new Applicative[Validated] {
        override def pure[T](x: T): Validated[T] = Valid(x)

        override def ap[A, B](vf: Validated[A => B])(va: Validated[A]): Validated[B] = {
          // map2(vf, va)((f, a) => f(a))
          (vf, va) match {
            case (Valid(f), Valid(data)) => Valid(f(data))
            case (Invalid(e), Valid(_)) => Invalid(e)
            case (Valid(_), Invalid(e)) => Invalid(e)
            case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2)
          }
        }

        override def map[A, B](va: Validated[A])(f: A => B): Validated[B] =
          ap(pure(f))(va)

        override def map2[A, B, Z](va: Validated[A], vb: Validated[B])(f: (A, B) => Z): Validated[Z] = {
          val g: A => B => Z = f.curried
          ap(ap(pure(g))(va))(vb)
        }

        override def map3[A, B, C, D](va: Validated[A], vb: Validated[B], vc: Validated[C])(f: (A, B, C) => D): Validated[D] = {
          val g: A => B => C => D = f.curried
          ap(ap(ap(pure(g))(va))(vb))(vc)
        }

        def tupled[A, B](va: Validated[A], vb: Validated[B]): Validated[(A, B)] = {
          map2(va, vb)((a, b) => (a, b))
        }
      }

    }


    val v1: Validated[Int] = Applicative[Validated].pure(1)
    val v2: Validated[Int] = Applicative[Validated].pure(2)
    val v3: Validated[Int] = Applicative[Validated].pure(3)
    val v4: Validated[Int] = Applicative[Validated].pure(3)
    val v5: Validated[Int] = Applicative[Validated].pure(3)
    val v6: Validated[Int] = Applicative[Validated].pure(3)
    val v7: Validated[Int] = Applicative[Validated].pure(3)

    (v1, v2, v3, v4, v5, v6, v7).mapN((a1, a2, a3, a4, a5, a6, a7) => a1 + a2 + a3 + a4 + a5 + a6 + a7)
  }



  /////////////////////////////////////////////////////////////////////////////////////


  object MonadApp {

    sealed trait MOption[+A] {
      def map[B](f: A => B): MOption[B]

      def flatMap[B](f: A => MOption[B]): MOption[B]
    }

    object MOption {
      implicit val catsMonad: Monad[MOption] = new Monad[MOption] {
        override def flatMap[A, B](fa: MOption[A])(f: A => MOption[B]): MOption[B] =
          fa match {
            case MSome(x) => f(x)
            case _ => MNone
          }

        @tailrec
        override def tailRecM[A, B](a: A)(f: A => MOption[Either[A, B]]): MOption[B] = {
          f(a) match {
            case MSome(e) => e match {
              case Right(r) => MSome(r)
              case Left(l) => tailRecM(l)(f)
            }
            case _ => MNone
          }
        }

        override def pure[A](x: A): MOption[A] = MSome(x)

        override def map[A, B](fa: MOption[A])(f: A => B): MOption[B] = flatMap(fa)(a => pure(f(a)))

        override def flatten[A](ffa: MOption[MOption[A]]): MOption[A] = {
          flatMap(ffa)(x => x)
        }

      }
    }

    case object MNone extends MOption[Nothing] {
      override def flatMap[B](f: Nothing => MOption[B]): MOption[B] = MNone

      override def map[B](f: Nothing => B): MOption[B] = MNone
    }

    case class MSome[+A](a: A) extends MOption[A] {
      override def flatMap[B](f: A => MOption[B]): MOption[B] =
        this match {
          case MSome(x) => f(x)
          case _ => MNone
        }

      override def map[B](f: A => B): MOption[B] = MSome(f(a))
    }


    case class Person(name: String)

    case class Account(balance: Double, owner: Person)

    case class Transfer(source: Account, dest: Account, amount: Double)

    def findPersonByName(name: String): MOption[Person] = ???

    def findAccountByPerson(person: Person): MOption[Account] = ???

    def findLastTransferBySourceAccount(account: Account): MOption[Transfer] = ???

    def findLastTransferByPersonName(name: String): MOption[Transfer] = {
      findPersonByName(name) match {
        case MSome(p) => findAccountByPerson(p) match {
          case MSome(a) => findLastTransferBySourceAccount(a)
          case _ => MNone
        }
        case _ => MNone
      }
    }

    def findLastTransferByPersonNameEnhanced(name: String): MOption[Transfer] =
      findPersonByName(name).flatMap(p => findAccountByPerson(p).flatMap(a => findLastTransferBySourceAccount(a)))

    def findLastTransferByPersonNameEvenBetter(name: String): MOption[Transfer] = {
      for {
        p <- findPersonByName(name)
        a <- findAccountByPerson(p)
        t <- findLastTransferBySourceAccount(a)
      } yield t
    }


  }


  /////////////////////////////////////////////////////////////////////////////////////


  object FoldableApp {

    trait MFoldable[F[_]] {
      def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B

      def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]

      def foldMap[A, B](fa: F[A])(f: A => B)(implicit M: Monoid[B]): B = {
        foldLeft(fa, M.empty)((b, a) => M.combine(b, f(a)))
      }
    }


    sealed trait MList[+A] {
      def foldRight[B](z: B)(f: (A, B) => B): B

      def foldLeft[B](z: B)(f: (B, A) => B): B
    }

    object MList {

      implicit val catsMonad: Monad[MList] = new Monad[MList] {
        override def flatMap[A, B](fa: MList[A])(f: A => MList[B]): MList[B] = ???

        override def tailRecM[A, B](a: A)(f: A => MList[Either[A, B]]): MList[B] = ???

        override def pure[A](x: A): MList[A] = ???
      }

      implicit val catsApplicative: Applicative[MList] = new Applicative[MList] {
        override def pure[A](x: A): MList[A] = ???

        override def ap[A, B](ff: MList[A => B])(fa: MList[A]): MList[B] = ???
      }

      implicit val catsFoldable: Foldable[MList] = new Foldable[MList] {
        override def foldLeft[A, B](fa: MList[A], b: B)(f: (B, A) => B): B = ???

        override def foldRight[A, B](fa: MList[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ???
      }
    }

    case object MNil extends MList[Nothing] {
      override def foldRight[B >: Nothing](z: B)(f: (Nothing, B) => B): B = z

      override def foldLeft[B](z: B)(f: (B, Nothing) => B): B = z
    }

    case class MCons[+A](head: A, tail: MList[A]) extends MList[A] {
      override def foldRight[B](z: B)(f: (A, B) => B): B = {

        @tailrec
        def foldRightTailrec(list: MList[A], acc: B): B = {
          list match {
            case MCons(h, t) => foldRightTailrec(t, f(h, z))
            case MNil => acc
          }
        }

        foldRightTailrec(this, z)
      }

      override def foldLeft[B](z: B)(f: (B, A) => B): B = {
        @tailrec
        def foldRightTailrec(list: MList[A], acc: B): B = {
          list match {
            case MCons(h, t) => foldRightTailrec(t, f(z, h))
            case MNil => acc
          }
        }

        foldRightTailrec(this, z)
      }
    }


    def sum(ints: MList[Int]): Int =
      ints match {
        case MNil => 0
        case MCons(head, tail) => head + sum(tail)
      }

    def sumEnhanced(ints: MList[Int]): Int =
      ints.foldRight(0)((x, z) => x + z)

    def length[A](list: MList[A]): Int =
      list match {
        case MNil => 0
        case MCons(_, tail) => 1 + length(tail)
      }

    def lengthEnhanced[A](list: MList[A]): Int =
      list.foldRight(0)((_, z) => z + 1)


    def filterPositive(ints: MList[Int], acc: MList[Int]): MList[Int] =
      ints match {
        case MNil => acc
        case MCons(head, tail) =>
          if (head >= 0) filterPositive(tail, MCons(head, acc))
          else filterPositive(tail, acc)
      }

    def filterPositiveEnhanced(ints: MList[Int]): MList[Int] =
      ints.foldRight[MList[Int]](MNil)((x, z) => if (x > 0) MCons(x, z) else z)

    case class Person(name: String)

    def findPersonByName(name: String): Option[Person] = ???

    def findPeopleByNames(names: MList[String]): Option[MList[Person]] = ???

    def traverse[F[_] : Applicative, A, B](as: MList[A])(f: A => F[B]): F[MList[B]] = {
      as match {
        case MNil => Applicative[F].pure(MNil)
        case MCons(h, t) => (f(h), traverse(t)(f)).mapN(MCons.apply)
      }
    }


  }


}
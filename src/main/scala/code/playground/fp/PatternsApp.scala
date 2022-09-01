package code.playground.fp

import scala.language.implicitConversions

object PatternsApp extends App {

  import cats._
  import cats.data._
  import cats.implicits._
//  import cats.syntax._


  case class Person(name: String, age: Int)

  case class Account(id: Long, balance: Double)

  // validation
  object ValidatedPattern {
    /*
      cats Validated[+E, +A] is used for validation.
      E is for errors and A is for computed value.
      E should be an instance of a Monoid that can combine errors
     */

    type IsValid[A] = Validated[List[String], A] // List[String] is the error accumulator

    /*
      the following are the examples of creating validated or invalidated values
     */
    10.valid
    "error happened".invalid
    "error happened".invalid[List[String]] // can have multiple errors

    val validated10 = 10.validNec[String] // wrap the errors in a NonEmtpyChain
    validated10.ensure(NonEmptyChain("Not Even"))(_ % 2 == 0)


    5.validNec[String].getOrElse(10) // result is 5
    "error happened".invalidNec[String].getOrElse(10) // result is 10


  }

  // dependency injection
  object DependencyInjectionPattern {

    case class DbClient() {
      def executeQuery[A](query: String): A = ???
    }

    def findAccountById(id: Long): DbClient => Account = ???

    def saveAccount(a: Account): DbClient => Boolean = ???

    def update(id: Long, f: Account => Account): DbClient => Boolean = {
      db => {
        val account = findAccountById(id)(db)
        val updatedAccount = f(account)
        saveAccount(updatedAccount)(db)
      }
    }


    def findAccountByIdEnhanced(id: Long): Reader[DbClient, Account] = ???

    def saveAccountEnhanced(a: Account): Reader[DbClient, Boolean] = ???


    def updateEnhanced(id: Long, f: Account => Account): Reader[DbClient, Boolean] = {
      for {
        account <- findAccountByIdEnhanced(id)
        result <- saveAccountEnhanced(f(account))
      } yield result
    }


    /*
      The Reader monad is a way to express a function
     */

    // this is a function from int to String (Int => String)
    def getSign: Reader[Int, String] = Reader(n => if (n > 0) "positive" else if (n < 0) "negative" else "zero")

    getSign(0)

    def getParity: Reader[Int, String] = Reader(n => if (n % 2 == 0) "even" else "odd")

    def getDescription: Reader[Int, String] =
      for {
        sign <- getSign
        parity <- getParity
      } yield s"sign: $sign, parity: $parity"

    getDescription(10)

    // to read the input from Reader
    def addOne: Reader[Int, Int] =
      for {
        value <- Reader((x: Int) => x)
      } yield value + 1


  }


  object TrackingPattern {

  }






}

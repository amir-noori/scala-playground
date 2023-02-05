package code.playground.cats


object BasicUsage {

  import cats.data.OptionT
  import cats.syntax.all._

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future


  object SimpleSyntax {
    implicit class ToFutureSuccessful[T](obj: T) {
      def asFuture: Future[T] = Future.successful(obj)
    }

    case class Account(id: Int)

    trait AccountService {
      def getAccountById(id: Int): Future[Option[Account]]
    }

    class AccountServiceImpl extends AccountService {
      override def getAccountById(id: Int): Future[Option[Account]] =
        Account(id).some.asFuture
    }
  }

  object MapSyntax {

    case class Person(name: String)

    def intFuture: Future[Int] = Future {
      10
    }

    def stringFuture: Future[String] = Future {
      "data"
    }

    def personFuture: Future[Person] = Future {
      Person("someone")
    }

    def processFutureData: Unit = {
      // the for comprehension is executed sequentially
      for {
        i <- intFuture
        s <- stringFuture
        p <- personFuture
      } yield s"$i $s $p"
    }

    def processFutureDataWithCats: Unit = {
      // cats mapN is executed concurrently
      (intFuture, stringFuture, personFuture).mapN {
        (i, s, p) =>
          s"$i $s $p"
      }
    }

  }

  object SequenceSyntax {
    val foo: List[Future[String]] = List(Future("hello"), Future("world"))
    val bar: Future[List[String]] = foo.sequence
  }

  object MonadSyntax {

    case class User(id: Int)

    def findUser(): Future[Option[User]] = Future {
      User(100).some
    }

    def findUserByCats(): OptionT[Future, User] = OptionT(Future {
      User(100).some
    })

    def processUser(): Unit = {
      findUser().map(maybeUser => {
        maybeUser.map(user => {
          print(s"user: $user")
        })
      })
    }

    def processUserByCats(): Unit =
      findUserByCats().map(user => print(s"user: $user"))

  }

  def main(args: Array[String]): Unit = {

  }

}

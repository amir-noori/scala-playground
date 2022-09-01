package code.playground.zio.scratch.old

object ZioPlayground extends App {

  //  object Template {
  //    import zio._
  //
  //    def apply(): Unit = {
  //
  //    }
  //  }

  object SimpleConsole {

    import zio._

    object myConsole {
      def putStrLn(line: => String) = ZIO.succeed(println(line))

      def getStrLn = ZIO.succeed(scala.io.StdIn.readLine())
    }

    val program = for {
      _ <- myConsole.putStrLn("What is your name?")
      name <- myConsole.getStrLn
      _ <- myConsole.putStrLn(s"Hello $name")
    } yield ()

    val program2 = for {
      _ <- console.putStrLn("What is your name?")
      name <- console.getStrLn
      _ <- console.putStrLn(s"Hello $name")
    } yield ()


    def apply(): Unit = {
      Runtime.default.unsafeRunToFuture(program)
      Runtime.default.unsafeRunToFuture(program2)

    }
  }

  //  SimpleConsole()

  object CreatingEffects {

    import zio._

    def apply(): Unit = {

      val zSuccessValue = ZIO.succeed(42)
      val zFailValue = ZIO.fail("failed")
      val zOption = ZIO.fromOption(Some(10))


      case class User(id: Int, teamId: Int)
      case class Team(id: Int)

      val maybeId: IO[Option[Nothing], Int] = ZIO.fromOption(Some(10))

      def getUser(userId: Int): IO[Throwable, Option[User]] = ???

      def getTeam(teamId: Int): IO[Throwable, Team] = ???

      val result: IO[Throwable, Option[(User, Team)]] = (for {
        userId <- maybeId
        user <- getUser(userId).some
        team <- getTeam(user.teamId).asSomeError
      } yield (user, team)).optional

    }
  }

  //  CreatingEffects()


  object EmailService {

    import zio._

    case class User(name: String, email: String)

    object UserEmailer {
      trait Service {
        def notifyUser(user: User, message: String): Task[Unit] // ZIO[Any, Throwable, Unit]
      }

      val serviceImpl: ULayer[Has[Service]] = ZLayer.succeed(new Service {
        override def notifyUser(user: User, message: String): Task[Unit] = Task {
          println(s"Sending $message to ${user.email}")
        }
      })

      def sendEmail(user: User, message: String): ZIO[Has[UserEmailer.Service], Throwable, Unit] =
        ZIO.accessM(hasService => hasService.get.notifyUser(user, message))
    }

    object UserDb {
      trait Service {
        def insert(user: User): Task[Unit]
      }

      val serviceImpl: ULayer[Has[Service]] = ZLayer.succeed(new Service {
        override def insert(user: User): Task[Unit] = Task {
          println(s"Inserting ${user.name}")
        }
      })

      def saveUser(user: User): ZIO[Has[UserDb.Service], Throwable, Unit] =
        ZIO.accessM(hasService => hasService.get.insert(user))

    }


    /*
      Horizontal Composition:

        ZLayer[R1, E1, A1] ++ ZLayer[R2, E2, A2] => ZLayer[R1 with R2, superType(E1, E2), A1 with A2]
        in our case:
          UserEmailer.Service ++ UserDb.Service = a service with sendEmail and insert methods

     */

    val horizontalCompositeService: ZLayer[Any, Throwable, Has[UserEmailer.Service] with Has[UserDb.Service]] =
      UserEmailer.serviceImpl ++ UserDb.serviceImpl

    /*
      Vertical Composition:

        feeding one service to the next as an input
     */

    object UserSubscription {
      class Service(notifier: UserEmailer.Service, userDb: UserDb.Service) {
        def subscribe(user: User): Task[User] = for {
          _ <- userDb.insert(user)
          _ <- notifier.notifyUser(user, s"User ${user.name} is subscribed.")
        } yield user
      }

      val composedService: ZLayer[Has[UserEmailer.Service] with Has[UserDb.Service], Nothing, Has[UserSubscription.Service]] =
        ZLayer.fromServices[UserEmailer.Service, UserDb.Service, UserSubscription.Service] { (userEmailer, userDb) =>
          new Service(userEmailer, userDb)
        }

      def subscribe(user: User): ZIO[Has[UserSubscription.Service], Throwable, User] =
        ZIO.accessM(_.get.subscribe(user))
    }

    val userSubscriptionLayer = horizontalCompositeService >>> UserSubscription.composedService


    def apply(): Unit = {
      val amir = User("number-6", "no6@prison.com")
      //      UserEmailer.sendEmail(amir, "this is a test message.") // it is a kind of effect
      //        .provideLayer(UserEmailer.serviceImpl) // dependency injection
      //        .exitCode

      UserSubscription
        .subscribe(amir)
        .provideLayer(userSubscriptionLayer)
        .exitCode

    }
  }

  //  EmailService()


  object ZioFibers {

    import zio._


    def apply(): Unit = {

    }
  }

  ZioFibers()


}

package code.playground.zio.scratch.old

case class Person(name: String, age: Int)

object Person {
  val peter: Person = Person("peter", 88)
}


trait ZIOApp {
  def run: ZIO[Any]

  def main(args: Array[String]): Unit = {
    run.run(result => {
      println(s"result is $result")
    })
    Thread.sleep(10000)
  }
}


object succeedNow extends ZIOApp {
  val peterZIO: ZIO[Person] = ZIO.succeedNow(Person.peter)

  override def run: ZIO[Person] = peterZIO
}

object succeedNowSideEffect extends ZIOApp {
  val hiZIO: ZIO[Unit] = ZIO.succeedNow(println("hello there!"))

  override def run: ZIO[Any] = ZIO.succeedNow(10)
}

object succeed extends ZIOApp {
  val hiZIO: ZIO[Unit] = ZIO.succeed(println("hello there!"))

  override def run: ZIO[Any] = ZIO.succeedNow(10)
}


object zip extends ZIOApp {
  val zippedZIO: ZIO[(Int, String)] = ZIO.succeed(10) zip ZIO.succeed("ten")

  override def run: ZIO[(Int, String)] = zippedZIO
}

object mapZio extends ZIOApp {
  val zippedZio: ZIO[(String, Int)] = ZIO.succeed("x") zip ZIO.succeed(10)

  override def run: ZIO[Any] =
    zippedZio.map { case (str, i) =>
      str * i
    }
}

object flatMapZio extends ZIOApp {
  val zippedZio: ZIO[(String, Int)] = ZIO.succeed("x") zip ZIO.succeed(10)

  def printLine(message: String): ZIO[Unit] =
    ZIO.succeed(println(message))

  override def run: ZIO[Any] =
    zippedZio.flatMap(tuple => printLine(s"tuple is: $tuple"))

}


object forComprehendZio extends ZIOApp {
  val zippedZio: ZIO[(String, Int)] = ZIO.succeed("x") zip ZIO.succeed(10)

  def printLine(message: String): ZIO[Unit] =
    ZIO.succeed(println(message))

  override def run: ZIO[Any] = for {
    t <- zippedZio
    _ <- printLine(s"tuple is: $t")
  } yield ()

}


object asyncZIOApp extends ZIOApp {
  val asyncZIO: ZIO[Int] = ZIO.async[Int] { complete =>
    println("async begin...") // this is eagerly evaluated
    Thread.sleep(3000)
    println("done")
    complete(10)
  }

  override def run: ZIO[Any] = asyncZIO
}


object forkZioApp extends ZIOApp {

  val asyncZIO1: ZIO[Int] = ZIO.async[Int] { complete =>
    println("async begin...1") // this is eagerly evaluated
    Thread.sleep(5000)
    println("async begin...1.1")
    complete(10)
  }

  val asyncZIO2: ZIO[Int] = ZIO.async[Int] { complete =>
    println("async begin...2") // this is eagerly evaluated
    Thread.sleep(3000)
    println("async begin...2.2")
    complete(10)
  }

  def printLine(message: String): ZIO[Unit] =
    ZIO.succeed(println(message))

  override def run: ZIO[Any] =
    for {
      fiber1 <- asyncZIO1.fork
      fiber2 <- asyncZIO2.fork
      _ <- printLine("Nice")
      int1 <- fiber1.join
      int2 <- fiber2.join
    } yield s"Done with values $int1, $int2"
}


object zipParZioApp extends ZIOApp {

  val asyncZio: ZIO[Int] = ZIO.async[Int] { complete =>
    println("begin")
    Thread.sleep(1000)
    complete(42)
  }

  override def run: ZIO[Any] = asyncZio parZip asyncZio
}



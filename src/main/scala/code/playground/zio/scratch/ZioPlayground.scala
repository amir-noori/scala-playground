package code.playground.zio.scratch

case class Person(name: String, age: Int)

object Person {
  val peter: Person = Person("Peter", 21)
}

trait ZIOApp {
  def run: ZIO[Any]

  def main(arg: Array[String]): Unit = run.run(value => {
    println(s"result is: $value")
  })
}


object succeedNow extends ZIOApp {

  val personZIO: ZIO[Person] = ZIO.succeedNow(Person.peter)

  override def run: ZIO[Any] = personZIO
}

object zipApp extends ZIOApp {
  val aZIO: ZIO[String] = ZIO.succeed("DATA0")
  val bZIO: ZIO[String] = ZIO.succeed("DATA1")
  val cZIO: ZIO[String] = ZIO.succeed("DATA2")
  val dZIO: ZIO[String] = ZIO.succeed("DATA3")
  val result: ZIO[(((String, String), String), String)] =
    aZIO.zip(bZIO).zip(cZIO).zip(dZIO)

  override def run: ZIO[Any] = {
    println(s"result steps: ${result.steps}")
    result
  }
}

object mapApp extends ZIOApp {
  val intZIO: ZIO[Int] = ZIO.succeed(10)

  override def run: ZIO[Any] = intZIO.map(a => a * 10)
}

object flatMapIssueApp extends ZIOApp {
  val zippedZIO = ZIO.succeed(10) zip ZIO.succeed("Data")

  def printLine(msg: String) = ZIO.succeed(println(msg))

  val mappedZIO = zippedZIO.map(tuple => printLine(s"tuple -> $tuple"))

  override def run: ZIO[Any] = mappedZIO
}

object flatMapApp extends ZIOApp {
  val zippedZIO = ZIO.succeed(10) zip ZIO.succeed("Data")

  def printLine(msg: String) = ZIO.succeed(println(msg))

  val mappedZIO: ZIO[Unit] = zippedZIO.flatMap(tuple => printLine(s"tuple -> $tuple"))

  override def run: ZIO[Any] = mappedZIO
}

object forComprehensionApp extends ZIOApp {
  def printLine(msg: String) = ZIO.succeed(println(msg))

  override def run: ZIO[Any] = for {
    tuple <- ZIO.succeed(10) zip ZIO.succeed("Data")
    _ <- printLine(s"tuple -> $tuple")
  } yield "Done"
}


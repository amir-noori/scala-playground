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

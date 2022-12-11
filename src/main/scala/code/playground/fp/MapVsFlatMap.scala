package code.playground.fp

object MapVsFlatMap extends App {

  def biggerThan10(a: Int): Option[Int] =
    if (a > 10) Some(a) else None

  //  val result = biggerThan10(20).map(n => {
  //    println("first")
  //    n - 15
  //  }).map(biggerThan10).flatMap(n => {
  //    println("second")
  //    n * 2
  //  })

  val result2 = biggerThan10(20).map(n => {
    println("first")
    n - 15
  }).flatMap(biggerThan10).map(n => {
    println("second")
    n * 2
  })

  //  println(s"result is: $result")
  //  println(s"result2 is: $result2")

  def parseInt(str: String): Option[Int] =
    scala.util.Try(str.toInt).toOption

  def divide(a: Int, b: Int): Option[Int] =
    if (b == 0) None else Some(a / b)


  def stringDivideBy1(aStr: String, bStr: String): Option[Int] =
    parseInt(aStr).map { aNum: Int =>
      parseInt(bStr).map { bNum: Int =>
        divide(aNum, bNum)
      }
    }.get.get

  def stringDivideBy1_1(aStr: String, bStr: String): Option[Int] = {
    val a = parseInt(aStr).get
    val b = parseInt(bStr).get
    divide(a, b)
  }

  def stringDivideBy2(aStr: String, bStr: String): Option[Int] =
    parseInt(aStr) flatMap { aNum: Int =>
      parseInt(bStr).flatMap { bNum: Int =>
        divide(aNum, bNum)
      }
    }

  def stringDivideBy2_1(aStr: String, bStr: String): Option[Int] =
    for {
      a <- parseInt(aStr)
      b <- parseInt(bStr)
      c <- divide(a, b)
    } yield c

  println(stringDivideBy1("40", "20"))
  println(stringDivideBy2("40", "20"))

}

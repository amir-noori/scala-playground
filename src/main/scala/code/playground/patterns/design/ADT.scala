package code.playground.patterns.design

object ADT extends App {

  /**
   *
   * SUM Types
   *
   */

  sealed abstract trait DayOfWeek
  case object Saturday extends DayOfWeek
  case object Sunday extends DayOfWeek
  case object Monday extends DayOfWeek
  case object Tuesday extends DayOfWeek
  case object Wednesday extends DayOfWeek
  case object Thursday extends DayOfWeek
  case object Friday extends DayOfWeek

  val aDay: DayOfWeek = Thursday

  aDay match {
    case Saturday => println("its Saturday")
    case Thursday => println("its thursday")
    case _ => println("some other day")
  }


  /**
   *
   * PRODUCT Types
   *
   */

  sealed case class RGB(red: Int, green: Int, blue: Int)

  val aColor = new RGB(10, 10, 10)

  aColor match {
    case RGB(10, 20, _) => println("first")
    case RGB(10, _, _) => println("second")
    case _ => println("other")
  }


}

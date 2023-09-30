package code.playground.challanges.projectEuler

import code.playground.challanges.projectEuler.Utils._

object Problem17 {

  val numbersMap = Map(
    0 -> "",
    1 -> "one",
    2 -> "two",
    3 -> "three",
    4 -> "four",
    5 -> "five",
    6 -> "six",
    7 -> "seven",
    8 -> "eight",
    9 -> "nine",
    10 -> "ten",

    11 -> "eleven",
    12 -> "twelve",
    13 -> "thirteen",
    14 -> "fourteen",
    15 -> "fifteen",
    16 -> "sixteen",
    17 -> "seventeen",
    18 -> "eighteen",
    19 -> "nineteen",
    20 -> "twenty",

    30 -> "thirty",
    40 -> "forty",
    50 -> "fifty",
    60 -> "sixty",
    70 -> "seventy",
    80 -> "eighty",
    90 -> "ninety",
    100 -> "hundred",

    1000 -> "thousand"
  )

  def solution(n: Int): String = {
    ""
  }

  /**
   *
   *
   *
   */
  def printDigits(digits: List[Int], result: String = "", hasPrevious: Boolean = false): String = {
    if (digits.isEmpty) return result

    (digits, digits.length) match {
      case (h :: t, l) =>
        if (l == 4) {
          printDigits(t, "one thousand", true)
        } else if (l == 3) {
          if (hasPrevious) {
            printDigits(t, result + numbersMap(h) + " hundred ", true)
          } else {
            printDigits(t, result + numbersMap(h) + " hundred ", true)
          }
        } else if (l == 2) {
          val and = if (hasPrevious) " " else ""

          numbersMap.get(h * 10 + t.head) match {
            case Some(value) => and + result + value
            case None => printDigits(t, and + result + numbersMap(h * 10), true)
          }
        } else {
          if (hasPrevious) printDigits(t, result + "-" + numbersMap(h), true)
          else numbersMap(h)
        }
    }

  }


  def main(args: Array[String]): Unit = {

    println(printDigits(extractDigits(1)))
    println(printDigits(extractDigits(2)))
    println(printDigits(extractDigits(10)))
    println(printDigits(extractDigits(11)))
    println(printDigits(extractDigits(20)))
    println(printDigits(extractDigits(21)))
    println(printDigits(extractDigits(100)))
    println(printDigits(extractDigits(101)))
    println(printDigits(extractDigits(118)))
    println(printDigits(extractDigits(123)))
    println(printDigits(extractDigits(1000)))

  }

}

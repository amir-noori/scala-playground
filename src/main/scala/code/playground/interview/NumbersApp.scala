package code.playground.interview

import java.util.{Calendar, NoSuchElementException}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random


object NumberOps {

  implicit class RichInt(n: Int) {
    def isPrime: Boolean = {
      if (n == 1) return false
      (2 to n / 2).foreach(x => if (n % x == 0) return false)
      true
    }

    def isPrimeRec: Boolean = {
      @tailrec
      def go(x: Int): Boolean = {
        if (x == 1) return true
        if (n % x == 0) return false
        else go(x - 1)
      }

      if (n == 1) return false
      go(n / 2)
    }

    // decompose a number to its prime divisors
    def decompose: List[Int] = {
      @tailrec
      def go(currentValue: Int, primeDivisors: List[Int]): List[Int] = {
        if (currentValue == 1) return primeDivisors
        if (n % currentValue == 0 && currentValue.isPrime) go(currentValue - 1, currentValue :: primeDivisors)
        else go(currentValue - 1, primeDivisors)
      }

      if (n == 1) return List()
      go(n / 2, List())
    }

    def decomposeEnhanced: List[Int] = {

      @tailrec
      def go(remaining: Int, currentDivisor: Int, primeDivisors: List[Int]): List[Int] = {
        if (currentDivisor > Math.sqrt(remaining)) return remaining :: primeDivisors
        else if (remaining % currentDivisor == 0) go(remaining / currentDivisor, currentDivisor, currentDivisor :: primeDivisors)
        else go(remaining, currentDivisor + 1, primeDivisors)
      }

      go(n, 2, List()).distinct
    }
  }


}

object NumbersApp extends App {

  import NumberOps._

  println(100.isPrime)
  println(2344.decompose)

  val x = Calendar.getInstance
  println(564213154.decomposeEnhanced)
  println(Calendar.getInstance.getTimeInMillis - x.getTimeInMillis)

  val y = Calendar.getInstance
  println(564213154.decompose)
  println(Calendar.getInstance.getTimeInMillis - x.getTimeInMillis)

}


object ApproximatePi extends App {

  def approximate(nPoints: Int): Double = {
    val random = new Random(System.currentTimeMillis())

    @tailrec
    def go(totalCount: Int, innerPoints: Int): Double = {
      if (totalCount == nPoints) return (innerPoints.asInstanceOf[Double] / nPoints.asInstanceOf[Double]) * 4
      val x: Double = random.nextDouble()
      val y: Double = random.nextDouble()
      if (isInCircle(x, y)) go(totalCount + 1, innerPoints + 1)
      else go(totalCount + 1, innerPoints)
    }

    go(0, 0)
  }

  def isInCircle(x: Double, y: Double): Boolean = {
    Math.sqrt(x * x + y * y) < 1
  }

  def approximateEnhanced(nPoints: Int): Double = {
    val random = new Random(System.currentTimeMillis())

    val innerPoints: Int = (1 to nPoints).map { n =>
      val x: Double = random.nextDouble()
      val y: Double = random.nextDouble()
      x * x + y * y
    }.count(x => x < 1)

    innerPoints * 4.0 / nPoints
  }

  println(approximate(1))
  println(approximate(2))
  println(approximate(5))
  println(approximate(10))
  println(approximate(100))
  println(approximate(1000))
  println(approximate(10000))
  println(approximate(100000))
  println(approximate(1000000))
  println(approximate(10000000))

  println(approximateEnhanced(1))
  println(approximateEnhanced(2))
  println(approximateEnhanced(5))
  println(approximateEnhanced(10))
  println(approximateEnhanced(100))
  println(approximateEnhanced(1000))
  println(approximateEnhanced(10000))
  println(approximateEnhanced(100000))
  println(approximateEnhanced(1000000))
  println(approximateEnhanced(10000000))

}


object RecurringDecimals extends App {

  def fractionToRecurringDecimal(n: Int, d: Int): List[Int] = {
    @tailrec
    def findFraction(num: Int, den: Int, fractions: List[Int], limit: Int): List[Int] = {
      if (limit == 0) return fractions
      val t: Int = num * 10
      val v: Int = t / den
      findFraction(t - v * den, den, v :: fractions, limit - 1)
    }

    findFraction(n, d, List(), limit = 10).reverse
  }

  println(fractionToRecurringDecimal(1, 333))


}


object LargestNumber extends App {

  /*
     [10, 9, 22, 3, 30, 7] = 973302210
   */

  def largestNumber(list: List[Int]): String = {
    @tailrec
    def getDigits(n: Int, acc: List[Int]): List[Int] = {
      if (n < 10) (n :: acc)
      else {
        val r = n % 10
        val d = n / 10
        getDigits(d, r :: acc)
      }
    }

    val orderedList = list.sorted(new Ordering[Int] {
      override def compare(x: Int, y: Int): Int = {
        @tailrec
        def compareTailrec(xds: List[Int], yds: List[Int]): Boolean = {
          if (xds.isEmpty && yds.nonEmpty) {
            return yds.head != 0
          }
          else if (xds.nonEmpty && yds.isEmpty) {
            return xds.head != 0
          }
          (xds, yds) match {
            case (n :: ns, m :: ms) =>
              if (n == m) compareTailrec(ns, ms)
              else n > m
          }
        }

        val xDigits: List[Int] = getDigits(x, List())
        val yDigits: List[Int] = getDigits(y, List())
        if (compareTailrec(xDigits, yDigits)) 1 else -1

      }
    })

    println(orderedList)
    orderedList.foldRight("")((acc, x) => s"$x$acc")
  }

  println(largestNumber(List(10, 9, 22, 3, 30, 7)))
}

object ReverseInt extends App {

  def reverseInt(number: Int): Int = {
    @tailrec
    def getDigits(n: Int, acc: List[Int]): List[Int] = {
      if (n < 10) n :: acc
      else getDigits(n / 10, n % 10 :: acc)
    }

    @tailrec
    def reverseTailrec(list: List[Int], result: Int, multiplier: Int): Int = {
      if (list.isEmpty) result
      else reverseTailrec(list.tail, result + list.head * multiplier, multiplier * 10)
    }

    reverseTailrec(getDigits(number, List()), 0, 1)
  }

  println(reverseInt(55466388))

}

object ParseInteger extends App {

  def parseChar(c: Char): Int = {
    c match {
      case '0' => 0
      case '1' => 1
      case '2' => 2
      case '3' => 3
      case '4' => 4
      case '5' => 5
      case '6' => 6
      case '7' => 7
      case '8' => 8
      case '9' => 9
      case _ => throw new IllegalArgumentException
    }
  }

  def parseInteger(str: String): Int = {
    @tailrec
    def innerParseTailrec(s: String, result: Int, multiplier: Int): Int = {
      if (s.isEmpty) result
      else innerParseTailrec(s.tail, result + parseChar(s.head) * multiplier, multiplier / 10)
    }

    val trimmed = str.trim
    if (trimmed.isEmpty) throw new IllegalArgumentException
    innerParseTailrec(trimmed, 0, Math.pow(10, trimmed.length - 1).toInt)
  }


  println(parseInteger("123"))
  println(parseInteger("346324"))
  println(parseInteger("36345346"))
}


object UglyNumber extends App {

  def uglyNumber(number: Int): Boolean = {

    @tailrec
    def isUgly(n: Int, divisor: Int): Boolean = {
      if (divisor > n / 2) return true

      if (divisor >= 7) return false
      if (n % divisor == 0) isUgly(n / divisor, 2)
      else {
        if (divisor == 2) isUgly(n, 3)
        else isUgly(n, divisor + 2)
      }
    }

    if (number == 1) return true
    isUgly(number, 2)
  }


  def uglyNumber_(number: Int): Boolean = {
    if (number == 1) true
    else if (number % 2 == 0) uglyNumber(number / 2)
    else if (number % 3 == 0) uglyNumber(number / 3)
    else if (number % 5 == 0) uglyNumber(number / 5)
    else false
  }

  def ithUgly(i: Int): Int = {

    @tailrec
    def findUglyNumber(currentNumber: Int, index: Int): Int = {
      if (i == index) currentNumber
      else if (uglyNumber_(currentNumber + 1)) findUglyNumber(currentNumber + 1, index + 1)
      else findUglyNumber(currentNumber + 1, index)
    }

    if (i == 0) return 2
    findUglyNumber(2, 0)
  }


  println(ithUgly(0))
  println(ithUgly(1))
  println(ithUgly(2))
  println(ithUgly(3))
  println(ithUgly(4))
  println(ithUgly(5))
  println(ithUgly(6))
  println(ithUgly(7))


}


object Duplicates extends App {

  // all members of the list appear exactly twice except one
  def duplicates_v1(list: List[Int]): Int = {
    list.find(x => list.count(n => n == x) == 1).head
  }

  def duplicates_v2(list: List[Int]): Int = {

    val map: mutable.Map[Int, Int] = mutable.Map[Int, Int]()
    list.foreach { x =>
      try {
        map(x) = map(x) + 1
      } catch {
        case _: NoSuchElementException => map(x) = 1
      }
    }
    map.find(_._2 == 1).head._1
  }

  def duplicates_v3(list: List[Int]): Int = {
    list.foldRight(0)(_ ^ _)
  }

  val list = (1 to 10000).toList ++ List(6454565) ++ (1 to 10000).toList

  val z = Calendar.getInstance
  println(duplicates_v3(list))
  println(Calendar.getInstance.getTimeInMillis - z.getTimeInMillis)


  val y = Calendar.getInstance
  println(duplicates_v2(list))
  println(Calendar.getInstance.getTimeInMillis - y.getTimeInMillis)


  val x = Calendar.getInstance
  println(duplicates_v1(list))
  println(Calendar.getInstance.getTimeInMillis - x.getTimeInMillis)


}





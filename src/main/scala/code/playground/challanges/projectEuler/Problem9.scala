package code.playground.challanges.projectEuler

import scala.Double.NaN

object Problem9 {

  def solution(n: Int): Int = {
    var result: Int = 0
    (1 to n).exists({ c =>
      val k = n - c
      val t = Math.sqrt(2 * c * c - k * k)
      val a1 = (k + t) / 2
      val a2 = (k - t) / 2
      val b1 = n - a1 - c
      val b2 = n - a2 - c
      val ok = !(a1.equals(NaN) || a2.equals(NaN) || b1.equals(NaN) || b2.equals(NaN)) &&
        a1 > 0 && a2 > 0 && b1 > 0 && b2 > 0 &&
        a1.isWhole && a2.isWhole && b1.isWhole && b2.isWhole

      if (ok) {
        val answer = (a1.toInt, a2.toInt, b1.toInt, b2.toInt, c)
        result = (a1 * b1 * c).toInt
        println(answer)
      }
      ok
    })

    result
  }

  def main(args: Array[String]): Unit = {
    println(solution(1000))
  }

}

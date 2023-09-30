package code.playground.challanges.projectEuler

import code.playground.challanges.projectEuler.Utils._

import scala.annotation.tailrec

/**
 * for the number n the sum is: n(n + 1) /2
 * for the sum k the prime factors are: (p1**m1)(p2**m2)...(pn**mn)
 * the divisor count = (m1 + 1) * (m2 + 1) ... (mn + 1)
 *
 */
object Problem12 {

  def solution(divisorThreshold: Int): Int = {

    @tailrec
    def go(upTo: Int = 0): Int = {
      (3 + upTo to 100 + upTo).find(n => {
        val tn = n * (n + 1) / 2
        val pfList = getPrimeFactorsAndRanks(tn)
        val divisorCount = pfList.foldLeft(1)((z, pf) => z * (pf.rank + 1))
        divisorCount >= divisorThreshold
      }) match {
        case Some(value) => value
        case None => go(upTo + 100)
      }
    }

    go()
  }

  def main(args: Array[String]): Unit = {
    solution(500)
  }

}

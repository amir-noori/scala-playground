package code.playground.challanges.projectEuler

import code.playground.challanges.projectEuler.Utils._

object Problem7 {

  def solution1(n: Int): BigInt =
    getFirstNPrimes(n).head

  def main(args: Array[String]): Unit = {
    println(solution1(10001))
  }

}

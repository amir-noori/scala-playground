package code.playground.challanges.projectEuler

import code.playground.challanges.projectEuler.Utils._
import code.playground.challanges.projectEuler.JavaUtils._

object Problem10 {

  /**
   * very slow (answer is: 142913828922)
   * as it progress it gets slower
   * @param n up to number n
   * @return
   */
  def solution1(n: Int): BigInt = getPrimesBelowN(n).sum

  /**
   *
   * 1179908154
   *
   * much faster solution using Sieve algorithm
   * as it progress it gets faster
   * @param n up to number n
   * @return
   */
  def solution2(n: Int): BigInt = getPrimeNumbersBySieveMethod(n).sum

  def solution3(n: Int): BigInt = getPrimeNumbersArrayBySieveMethod(n).sum


  def main(args: Array[String]): Unit = {
    println(solution2(2000000))
  }

}

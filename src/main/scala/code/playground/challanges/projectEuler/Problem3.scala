package code.playground.challanges.projectEuler

import code.playground.challanges.projectEuler.Utils.getNextPrime

import scala.annotation.tailrec

object Problem3 {

  @tailrec
  def findLargestPrimeFactor(value: BigInt, largestPrime: BigInt = 2): BigInt =
    if (largestPrime >= value) value
    else if (value % largestPrime == 0) findLargestPrimeFactor(value / largestPrime, largestPrime)
    else findLargestPrimeFactor(value, getNextPrime(largestPrime))

  def main(args: Array[String]): Unit = {
    println(findLargestPrimeFactor(600851475143L))
  }

}

package code.playground.challanges.projectEuler

import scala.annotation.tailrec

object Utils {

  @tailrec
  def extractDigits(n: Int, acc: List[Int] = List()): List[Int] =
    if (n / 10 != 0) extractDigits(n / 10, n % 10 :: acc)
    else n :: acc

  @tailrec
  def getNextPrime(n: BigInt): BigInt =
    (n + 1 to n + 100).find(isPrime) match {
      case None => getNextPrime(n + 100)
      case Some(value) => value
    }

  def getNextPrime(n: Int): Int = getNextPrime(BigInt(n)).toInt

  def isPrime(n: BigInt): Boolean =
    !(BigInt(2) to n / 2).exists(i => n % i == 0)

  def getPrimeFactors(n: Int): Seq[Int] = {

    @tailrec
    def go(currentPrime: Int, value: Int, acc: Seq[Int] = List()): Seq[Int] = {
      if (currentPrime >= value) return acc
      if (value % currentPrime != 0) {
        val next = getNextPrime(currentPrime)
        go(next, n, acc)
      }
      else go(currentPrime, value / currentPrime, acc :+ currentPrime)
    }

    go(2, n)
  }
}

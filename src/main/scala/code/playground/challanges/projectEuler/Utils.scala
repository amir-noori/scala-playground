package code.playground.challanges.projectEuler

import scala.annotation.tailrec

object Utils {

  case class Factor(prime: Int, rank: Int)


  @tailrec
  def extractDigits(n: Int, acc: List[Int] = List()): List[Int] =
    if (n / 10 != 0) extractDigits(n / 10, n % 10 :: acc)
    else n :: acc

  @tailrec
  def getNextPrime(n: BigInt, previousPrimes: List[BigInt] = List()): BigInt =
    (n + 1 to n + 100).find(i => isPrime(i, previousPrimes)) match {
      case None => getNextPrime(n + 100, previousPrimes)
      case Some(value) => value
    }

  def getNextPrime(n: Int): Int = getNextPrime(BigInt(n)).toInt

  def getFirstNPrimes(n: BigInt): List[BigInt] = {

    @tailrec
    def go(i: BigInt = 0, currentPrime: BigInt = 2, acc: List[BigInt] = List()): List[BigInt] = {
      if (i == n) return acc
      go(i + 1, getNextPrime(currentPrime, acc), currentPrime :: acc)
    }

    go()
  }

  def isPrime(n: BigInt): Boolean =
    !(BigInt(2) to Math.sqrt(n.toInt).toInt).exists(i => n % i == 0)

  def isPrime(n: BigInt, previousPrimes: List[BigInt]): Boolean = {
    val sqrt = BigInt(Math.sqrt(n.toInt).toInt)
    !previousPrimes.exists(p => p <= sqrt && n % p == 0)
  }

  def getPrimesBelowN(n: Int): List[BigInt] = {

    @tailrec
    def go(currentPrime: BigInt = 2, acc: List[BigInt] = List()): List[BigInt] = {
      if (currentPrime <= n) go(getNextPrime(currentPrime, acc), currentPrime +: acc)
      else acc
    }

    go()
  }

  def getPrimeFactors(n: Int): scala.collection.mutable.Seq[Int] =
    getPrimeFactorsAndRanks(n).map(pf => pf.prime)

  def getPrimeFactorsAndRanks(n: Int): scala.collection.mutable.Seq[Factor] = {

    import scala.collection.mutable.Seq

    @tailrec
    def go(currentPrime: Int, value: Int, acc: Seq[Factor] = Seq[Factor]()): Seq[Factor] = {
      if (currentPrime > value) return acc
      if (value % currentPrime != 0) {
        val next = getNextPrime(currentPrime)
        go(next, value, acc)
      } else if (acc.isEmpty) {
        go(currentPrime, value / currentPrime, Seq(Factor(currentPrime, 1)))
      } else {
        val update = acc.last match {
          case Factor(prime, rank) if prime == currentPrime =>
            acc.updated(acc.length - 1, Factor(prime, rank + 1))
          case _ => acc :+ Factor(currentPrime, 1)
        }
        go(currentPrime, value / currentPrime, update)
      }
    }

    go(2, n)
  }

  def getPrimeNumbersBySieveMethod(n: Int): Seq[Int] = {
    @tailrec
    def go(p: Int, nums: Seq[Int], primes: Seq[Int]): Seq[Int] = {
      if (p < n) {
        val filteredAcc = nums.filter(i => p < i && i % p != 0)
        if (filteredAcc.isEmpty) primes
        else go(filteredAcc.last, filteredAcc, filteredAcc.last +: primes)
      }
      else primes
    }

    go(2, (2 to n).foldLeft(Seq[Int]())((z, i) => i +: z), Seq(2))
  }


}

package code.playground.challanges.projectEuler

import code.playground.challanges.projectEuler.Utils.extractDigits

object Problem4 {

  def findPalindromicNum(): IndexedSeq[Int] =
    (100 * 100 to 999 * 999)
      .filter(i => isPalindromic(i) && isProductOfTwo_n_DigitsNumbers(i, 3))

  def isPalindromic(n: Int): Boolean = {
    val l = extractDigits(n)
    !(0 to l.length / 2).exists(i => l(i) != l(l.length - i - 1))
  }

  def isProductOfTwo_n_DigitsNumbers(value: Int, n: Int): Boolean =
    (100 to 999).exists(i => value % i == 0 && (value / i).toString.length == n)

  def main(args: Array[String]): Unit = {
    findPalindromicNum()
  }
}

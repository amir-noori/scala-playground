package code.playground.challanges.projectEuler

import scala.annotation.tailrec

object Problem2 {


  def solution1(n: Int): Seq[Int] =
    (1 to n).filter(i => i % 3 == 0 || i % 5 == 0)

  def fibEvenSum(n: Int): BigInt = {

    @tailrec
    def go(m: Int, prev2: Int, prev: Int, acc: BigInt): BigInt = {
      if (m == n || acc >= 4000000) acc
      else {
        val r = prev + prev2
        if (r % 2 == 0) go(m + 1, prev, r, acc + r)
        else go(m + 1, prev, r, acc)
      }
    }

    go(2, 0, 1, 0)
  }

  def main(args: Array[String]): Unit = {
    println(fibEvenSum(1000))
  }

}

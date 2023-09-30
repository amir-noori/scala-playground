package code.playground.challanges.projectEuler

import scala.annotation.tailrec


object Problem14 {

  def solution(n: Long): (Long, Long) = {

    @tailrec
    def go(i: Long, longestChainIndex: Long, longestLength: Long, nextInChain: Long, length: Long): (Long, Long) = {
      if (nextInChain == 1) {
        if (i == n) (longestChainIndex, longestLength)
        else if (length > longestLength) go(i + 1, i, length, i + 1, 1)
        else go(i + 1, longestChainIndex, longestLength, i + 1, 1)
      } else if (nextInChain % 2 == 0) {
        go(i, longestChainIndex, longestLength, nextInChain / 2, length + 1)
      } else {
        go(i, longestChainIndex, longestLength, 3 * nextInChain + 1, length + 1)
      }
    }

    go(1, 1, 1, 1, 1)
  }


  def main(args: Array[String]): Unit = {
    println(solution(1000000))
  }

}

package code.playground.challanges

import scala.annotation.tailrec

object ThreeLetters {

  def solution(a: Int, b: Int): String = {

    var c1: Char = 'a'
    var c2: Char = 'b'
    if (a <= b) {
      c1 = 'b'
      c2 = 'a'
    }

    @tailrec
    def solve(result: String, count1: Int, count2: Int, totalCount: Int, chooseTwo: Boolean): String = {
      if (totalCount <= 0) return result
      if (chooseTwo)
        solve(result + c1 + c1 + c2,
          count1 - 2,
          count2 - 1,
          totalCount - 3,
          count2 >= count1)
      else
        solve(
          result + c1 + c2,
          count1 - 1,
          count2 - 1,
          totalCount - 2,
          chooseTwo = false)
    }

    if (a > b) solve("", a, b, a + b, chooseTwo = true)
    else solve("", b, a, a + b, chooseTwo = true)
  }

  def main(args: Array[String]): Unit = {
    println(solution(10, 5))
    println(solution(10, 7))
    println(solution(5, 10))
    println(solution(7, 10))
  }

}

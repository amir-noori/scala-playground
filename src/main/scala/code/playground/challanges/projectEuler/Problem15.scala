package code.playground.challanges.projectEuler


/**
 * for n x n tiles each move to left is 1 and down is 0
 * the total moves are n moves to right and n moves down
 * if we can specify in how many ways moves to right (n) can be chosen out of total moves (2n) solution is found.
 * answer = (2n)!/(n! n!) = (n + 1)(n + 2)...(2n) / n!
 */
object Problem15 {

  def solution(n: BigInt): BigInt =
    (n + 1 to 2 * n).product / (BigInt(1) to n).product


  def main(args: Array[String]): Unit = {
    println(solution(2))
    println(solution(3))
    println(solution(20))
  }

}

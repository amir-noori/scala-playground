package code.playground.challanges.projectEuler

object Problem1 {

  def solution1(n: Int): Int =
    (1 to n).filter(i => i % 3 == 0 || i % 5 == 0).sum

  def main(args: Array[String]): Unit = {
    println(solution1(999))
  }
}

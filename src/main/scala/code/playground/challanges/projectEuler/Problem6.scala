package code.playground.challanges.projectEuler

object Problem6 {

  def solution1(n: Int): Int = {
    val x = (1 to n).map(i => i * i).sum
    val y = (1 to n).sum
    y * y - x
  }

  def solution2(n: Int): Int =
    (Math.pow(n * (n + 1) / 2, 2) - (n * (n + 1) * (2 * n + 1)) / 6).toInt

  def main(args: Array[String]): Unit = {
    println(solution2(10))
  }

}

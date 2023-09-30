package code.playground.challanges.projectEuler


object Problem16 {

  def solution(n: BigInt): Int =
    (BigInt(1) to n).foldLeft(BigInt(1))((z, _) => z * 2)
      .toString().foldLeft(0)((z, i) => z + i.toInt - 48)

  def main(args: Array[String]): Unit = {
    println(solution(1000))
  }

}

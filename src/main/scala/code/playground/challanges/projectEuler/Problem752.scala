package code.playground.challanges.projectEuler

import scala.annotation.tailrec

object Problem752 {

  case class P(a: BigInt, b: BigInt)

  val cache = scala.collection.mutable.Map[Int, Int]()

  def calculate(n: BigInt): Unit = {
    @tailrec
    def go(m: Int, current: P): Unit = {
      if(m == n) return
      val a = current.a + 7 * current.b
      val b = current.a + current.b
      // println(s"$a + ${b}rad(7)")
      go(m + 1, P(a, b))
    }

    go(0, P(1, 1))
  }

  def main(args: Array[String]): Unit = {
    calculate(10)
  }
}

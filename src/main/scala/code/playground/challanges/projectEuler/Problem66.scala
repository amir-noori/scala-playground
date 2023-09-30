package code.playground.challanges.projectEuler

import scala.util.control.Breaks._

object Problem66 {

  def solution() = {
    (3 to 1000).map(d => {
      if (!Math.sqrt(d.toDouble).isWhole) {
        if (d % 2 == 0) check(d, true)
        else check(d, false)
      } else (BigDecimal(0), BigDecimal(0))
    }).foldLeft(BigDecimal(0))((max, i) => if (i._1 > max) i._1 else max)
  }

  def check(d: BigDecimal, isEven: Boolean): (BigDecimal, BigDecimal) = {
    var n: BigDecimal = 2
    var x1: BigDecimal = 0
    var y1: BigDecimal = 0
    breakable {
      while (true) {
        val x = if (isEven) 2 * n.toDouble + 1 else n.toDouble
        val a: BigDecimal = (x * x - 1) / d
        val y: Double = Math.sqrt(a.toDouble)
        if (y != 0 && y.isWhole && a.isWhole) {
          x1 = x
          y1 = y
          break
        }
        n = n + 1
      }
    }

    println(s"d= $d, x= ${x1.toLong}, y= ${y1.toLong}")
    (x1, y1)
  }

  def main(args: Array[String]): Unit = {
    println(solution())
  }

}

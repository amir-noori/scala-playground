package code.playground.exercises.stdlib

import scala.annotation.tailrec

object Fib extends App {

  def fib_(n: Int): Double = {
    if (n == 0 || n == 1) n
    else fib_(n - 1) + fib_(n - 2)
  }

  def fib(n: Int): Double = {
    @tailrec
    def fibTailrec(m: Int, result: Double, m_1: Double, m_2: Double): Double = {
      if (m == n) result
      else fibTailrec(m + 1, result + m_1, result, m_1)
    }

//    if (n == 0 || n == 1) return n
    fibTailrec(2, 1, 1, 0)
  }

  println(fib(1000))

}

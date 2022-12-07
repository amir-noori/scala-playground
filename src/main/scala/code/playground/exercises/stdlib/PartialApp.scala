package code.playground.exercises.stdlib

import scala.annotation.tailrec

object PartialApp extends App {

  def sum(a: Int, b: Int, c: Int) = a + b + c

  val sum3 = sum _

  print(sum3(1, 9, 7))

  val doubleEvens: PartialFunction[Int, Int] =
    new PartialFunction[Int, Int] {
      //States that this partial function will take on the task
      def isDefinedAt(x: Int) = x % 2 == 0

      //What we do if this partial function matches
      def apply(v1: Int) = v1 * 2
    }

  def applyToList[A](l: List[A], p: A => Boolean, f: A => A): List[A] = {
    @tailrec
    def applyRex(theList: List[A], result: List[A]): List[A] = {
      theList match {
        case Nil => result
        case h :: t =>
          if (p(h)) applyRex(t, f(h) :: result) else applyRex(t, h :: result)
      }
    }

    applyRex(l, List()).reverse
  }

  println(doubleEvens(10))
  println(doubleEvens(3))
  println(applyToList[Int](List(1, 10, 2, 3, 4, 22, 17), x => x % 2 == 0, x => x * 2))
  println(List(1, 10, 2, 3, 4, 22, 17).map(x => if (x % 2 == 0) x * 2 else x))

}

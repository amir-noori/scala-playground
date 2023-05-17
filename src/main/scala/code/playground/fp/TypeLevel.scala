package code.playground.fp


object TypeLevel {

//  import scala.reflect.runtime.universe._
//
//  def show[T](value: T)(implicit tag: TypeTag[T]) =
//    tag.toString.replace("code.playground.fp.TypeLevel", "")
//
//
//  class Nat
//
//  class Succ[N <: Nat] extends Nat
//
//  class _0 extends Nat
//
//  type _1 = Succ[_0]
//  type _2 = Succ[_1]
//  type _3 = Succ[_2]
//  type _4 = Succ[_3]
//  type _5 = Succ[_4]
//
//  trait <[A <: Nat, B <: Nat]
//
//  object < {
//    implicit def ltBasic[A <: Nat]: <[_0, Succ[A]] = new <[_0, Succ[A]] {}
//
//    implicit def ltRec[A <: Nat, B <: Nat](implicit lt: <[A, B]): <[Succ[A], Succ[B]] = new <[Succ[A], Succ[B]] {}
//
//    def apply[A <: Nat, B <: Nat](implicit lt: <[A, B]): A < B = lt
//  }
//
//  trait <=[A <: Nat, B <: Nat]
//
//  object <= {
//    implicit def ltBasic[A <: Nat]: <=[_0, A] = new <=[_0, A] {}
//
//    implicit def ltRec[A <: Nat, B <: Nat](implicit lte: <=[A, B]): <=[Succ[A], Succ[B]] = new <=[Succ[A], Succ[B]] {}
//
//    def apply[A <: Nat, B <: Nat](implicit lte: <=[A, B]): A <= B = lte
//  }
//
//  trait +++[A <: Nat, B <: Nat, S <: Nat]
//
//  object +++ {
//    implicit def plusBasicLeft[A <: Nat]: +++[A, _0, A] = new +++[A, _0, A] {}
//
//    implicit def plusBasicRight[A <: Nat]: +++[_0, A, A] = new +++[_0, A, A] {}
//
//    implicit def plusRec[A <: Nat, B <: Nat](implicit add: +++[A, Succ[B]]): +++[Succ[A], B] = new +++[Succ[A], B] {}
//
//    def apply[A <: Nat, B <: Nat](implicit add: +++[A, B]): A +++ B = add
//  }
//
//  val zeroOneComparison: <[_0, _1] = <[_0, _1]
//  val incrementedByOneComparison: <[_2, _3] = <[_2, _3]
//  val comparison: _1 < _3 = <[_1, _3]
//  // val invalidComparison: <[_3, _1] = <[_3, _1]
//
//  val zeroOneComparisonLte: <=[_0, _1] = <=[_0, _1]
//  val incrementedByOneComparisonLte: <=[_2, _3] = <=[_2, _3]
//  val comparisonLte: _1 <= _3 = <=[_1, _3]
//
//  val zeroOneAdd: +++[_0, _1] = +++[_0, _1]
//  //  val zeros: +++[_0, _0] = +++[_0, _0]
//  val incrementedByOneAdd: +++[_2, _3] = +++[_2, _3]
//  //  val incrementedByOneAdd2: +++[_1, _0] = +++[_1, _0]
//  val adding1: _1 +++ _3 = +++[_1, _3]
//  val adding2: _3 +++ _1 = +++[_3, _1]
//
//
//  def main(args: Array[String]): Unit = {
//    println(show(zeroOneAdd))
//    println(show(incrementedByOneAdd))
//    println(show(adding1))
//    println(show(adding2))
//  }


}

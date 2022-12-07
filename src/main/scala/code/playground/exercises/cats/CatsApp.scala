package code.playground.exercises.cats

object SemigroupApp extends App {

  import cats.Semigroup

  val a: Int = Semigroup[Int].combine(10, 20)
  println(s"a -> $a")

  val b: List[Int] = Semigroup[List[Int]].combine(List(1, 2, 3), List(5, 4, 6))
  println(s"b -> $b")

  val f: Int => Int = Semigroup[Int => Int].combine(x => x + 1, x => x * 2)
  val g: Int => Int = Semigroup[Int => Int].combine(x => x * 2, x => x + 1)
  println(s"f(10) -> ${f(10)}")
  println(s"g(10) -> ${g(10)}")

  import cats.implicits._

  Map("foo" -> Map("bar" -> 5)).combine(Map("foo" -> Map("bar" -> 6), "baz" -> Map()))
  Map("foo" -> List(1, 2)).combine(Map("foo" -> List(3, 4), "bar" -> List(42)))

}


object MonoidApp extends App {



}





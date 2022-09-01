package code.playground.odersky

import scala.language.implicitConversions

object Ch6 extends App {

  class Rational(n: Int, d: Int) {
    require(d != 0)

    private val g = gcd(n, d)
    val numer: Int = n / g
    val denom: Int = d / g

    def this(n: Int) = this(n, 1)

    override def toString: String = numer + "/" + denom

    def +(that: Rational): Rational =
      new Rational(this.numer * that.denom + that.numer * this.denom, this.denom * that.denom)

    def *(that: Rational): Rational =
      new Rational(numer * that.numer, denom * that.denom)

    def *(that: Int): Rational =
      new Rational(numer * that, denom)

    private def gcd(a: Int, b: Int): Int =
      if (b == 0) a else gcd(b, a % b)
  }

  object Rational {
    implicit def convertIntToRational(value: Int): Rational = new Rational(value)
  }

  val a = new Rational(100, 40)
  val b = new Rational(100, 40)
  val c = a + b
  println(c)

  println(a * 10)


  // to make this work we need the implicit method in Rational companion object to convert Int to Rational
  println(10 * a)

}

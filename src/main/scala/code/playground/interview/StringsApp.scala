package code.playground.interview

import java.util.NoSuchElementException
import scala.annotation.tailrec
import scala.collection.mutable


object StringsApp extends App {

  def countEachCharacter_v1(str: String): Map[Char, Int] = {
    val map: mutable.Map[Char, Int] = mutable.Map[Char, Int]()

    str.foreach(s => {
      try {
        map(s) = map(s) + 1
      } catch {
        case _: NoSuchElementException => map(s) = 1
      }
    })

    map.toMap
  }


  def countEachCharacter_v2(str: String): Map[Char, Int] = {
    @tailrec
    def countTailrec(s: String, acc: Map[Char, Int]): Map[Char, Int] = {
      if (s.isEmpty) acc
      else {
        val head = s.head
        if (acc.contains(head)) countTailrec(s.tail, acc + (head -> (acc(head) + 1)))
        else countTailrec(s.tail, acc + (head -> 1))
      }
    }

    countTailrec(str, Map())
  }

  def checkAnagrams(s1: String, s2: String): Boolean = {
    val m1: Map[Char, Int] = countEachCharacter_v1(s1)
    val m2: Map[Char, Int] = countEachCharacter_v1(s2)

    m1.view.foreach(x => {
      try {
        val v1: Int = m2(x._1)
        val v2: Int = x._2
        if (v1 != v2) return false
      } catch {
        case _: NoSuchElementException => return false
      }
    })
    true
  }

  def checkAnagrams_(s1: String, s2: String): Boolean = {
    countEachCharacter_v1(s1) == countEachCharacter_v1(s2)
  }

  def checkAnagrams__(s1: String, s2: String): Boolean = s1.sorted == s2.sorted

  println(checkAnagrams("ADDSSX", "DDAXSS"))
  println(checkAnagrams_("ADDSSX", "DDAXSS"))
  println(checkAnagrams__("ADDSSX", "DDAXSS"))

  //  val str = "test" * 10000
  //
  //  val y = Calendar.getInstance
  //  println(countEachCharacter_v2(str))
  //  println(Calendar.getInstance.getTimeInMillis - y.getTimeInMillis)
  //
  //
  //  val z = Calendar.getInstance
  //  println(countEachCharacter_v1(str))
  //  println(Calendar.getInstance.getTimeInMillis - z.getTimeInMillis)


}


object Parenthesis extends App {

  def hasValidParenthesis(str: String): Boolean = {

    /*
      check("((()))", []) ->
      check("(()))", [(, ]) ->
      check("()))", [(, (]) ->
      check(")))", [(, (, (]) ->
      check("))", [(, (]) ->
      check(")", [(]) ->
      check("", [])
     */

    @tailrec
    def check(s: String, acc: List[Char]): List[Char] = {
      if (s.isEmpty) acc
      else {
        val head: Char = s.head
        if (head == '(') check(s.tail, '(' :: acc)
        else if (head == ')') check(s.tail, acc.tail)
        else check(s.tail, acc)
      }
    }

    check(str, List()).isEmpty
  }

  //  println(hasValidParenthesis("()"))
  //  println(hasValidParenthesis("()()"))
  //  println(hasValidParenthesis("((()()))"))
  //  println(hasValidParenthesis("(((test)()test))"))
  //  println(hasValidParenthesis("((()"))

  /*
    e.g:
      n = 1 => ["()"]
      n = 2 => ["()()", "(())"]
      n = 3 => ["()()()", "()(())", "(())()", "((()))", "(()())"]
   */
  def generateAllValidParenthesis(numberOfParenthesis: Int): Set[String] = {

    @tailrec
    def getCombinations(acc: Set[String], str: String, originalStr: String, i: Int): Set[String] = {
      if (str.isEmpty) acc
      else {
        val (left, right) = originalStr.splitAt(i)
        val newStr = left ++ "()" ++ right
        getCombinations(acc + newStr, str.tail, originalStr, i + 1)
      }
    }

    @tailrec
    def generate(n: Int, acc: Set[String]): Set[String] = {
      if (n == 1) acc
      else {
        var allCombinations: Set[String] = Set[String]()
        acc.foreach { a =>
          allCombinations = allCombinations ++ getCombinations(Set[String](), a, a, 0)
        }
        generate(n - 1, allCombinations)
      }
    }

    generate(numberOfParenthesis, Set[String]("()"))

  }

  def generateAllValidParenthesis2(numberOfParenthesis: Int): Set[String] = {

    @tailrec
    def genParensTailrec(nRemainingParens: Int, currentStrings: Set[String]): Set[String] = {
      if (nRemainingParens == 0) currentStrings
      else {
        val newStrings = for {
          string <- currentStrings
          index <- 0 until string.length
        } yield {
          val (before, after) = string.splitAt(index)
          s"$before()$after"
        }
        genParensTailrec(nRemainingParens - 1, newStrings)
      }
    }

    if (numberOfParenthesis == 0) Set()
    else genParensTailrec(numberOfParenthesis - 1, Set("()"))
  }


  //  println(generateAllValidParenthesis(1).size)
  //  println(generateAllValidParenthesis(2).size)
  //  println(generateAllValidParenthesis(3).size)
  //  println(generateAllValidParenthesis(12).size)
  //
  //  println(generateAllValidParenthesis2(1).size)
  //  println(generateAllValidParenthesis2(2).size)
  //  println(generateAllValidParenthesis2(3).size)
  //  println(generateAllValidParenthesis2(12).size)


}





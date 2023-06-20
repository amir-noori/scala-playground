package code.playground.challanges

import scala.annotation.tailrec

object PolygonConcavityIndex {

  case class Point2D(x: Int, y: Int)

  def solution(points: Array[Point2D]): Int = {

    val PI_ATAN = Math.atan(Math.PI)
    var theta: Double = 0

    @tailrec
    def solve(index: Int): Int = {
      val last = index + 2

      if (last > points.length) return -1

      if (last == points.length)
        theta = calculateTheta(points(index), points(index + 1), points(0))
      else if (last == points.length + 1)
        theta = calculateTheta(points(index), points(0), points(1))
      else
        theta = calculateTheta(points(index), points(index + 1), points(index + 2))

      if (theta > PI_ATAN) {
        println(s"concave at index ${index + 1}")
        index + 1
      } else {
        println(s"next[$index] ${points(index)}")
        solve(index + 1)
      }

    }

    solve(0)
  }

  def calculateTheta(p1: Point2D, p2: Point2D, p3: Point2D): Double = {
    val m1 = calculateSlope(p1, p2)
    val m2 = calculateSlope(p2, p3)
    println(s"m1[$m1], m2[$m2]")
    Math.atan(Math.abs((m1 - m2) / (1 + m1 * m2)))
  }

  def calculateSlope(p1: Point2D, p2: Point2D): Double = {
    val denom = p2.x - p1.x
    if (denom == 0) Double.NaN
    else (p2.y - p1.y) / denom
  }

  def main(args: Array[String]): Unit = {
    //    val a1 = Array(Point2D(-1, 3), Point2D(3, 1), Point2D(0, -1), Point2D(-2, 1))
    //    println(solution(a1))

    val a2 = Array(Point2D(-1, 3), Point2D(1, 2), Point2D(1, 1), Point2D(3, 1),
      Point2D(0, -1), Point2D(-2, 1), Point2D(-1, 2))
    println(solution(a2))

  }

}


object PolygonConcavityIndex2 {

  case class CustomPoint2D(x: Double, y: Double)

  case class Point2D(x: Int, y: Int)

  case class Line2D(p1: CustomPoint2D, p2: CustomPoint2D)

  def solution(a: Array[Point2D]): Int = {
    val customArray = a.map(p => CustomPoint2D(p.x, p.y))
    customSolution(customArray)
  }

  def customSolution(points: Array[CustomPoint2D]): Int = {

    val center = getMiddlePoint(points)
    println(s"center $center")

    @tailrec
    def solve(index: Int): Int = {
      val last = index + 2
      var p1: CustomPoint2D = null
      var p2: CustomPoint2D = null
      var p3: CustomPoint2D = null
      var concaveIndex: Int = 0

      if (last > points.length) return -1

      if (last == points.length) {
        p1 = points(index)
        p2 = points(index + 1)
        p3 = points(0)
        concaveIndex = index + 1
      }
      else if (last == points.length + 1) {
        p1 = points(index)
        p2 = points(0)
        p3 = points(1)
        concaveIndex = 0
      }
      else {
        p1 = points(index)
        p2 = points(index + 1)
        p3 = points(index + 2)
        concaveIndex = index + 1
      }

      val middle = getMiddlePoint(Array(p1, p2, p3))
      val line = Line2D(middle, center)
      val edge1 = Line2D(p1, p2)
      val edge2 = Line2D(p2, p3)

      val crossPoint1 = findCrossPoint(line, edge1)
      val crossPoint2 = findCrossPoint(line, edge2)

      if (!crossPoint1.equals(crossPoint2) && (checkConcavity(line, crossPoint1) || checkConcavity(line, crossPoint2))) {
        println(s"Concave at $p2")
        return concaveIndex
      }
      else {
        println(
          s"""
           P1: $p1, P2: $p2, P3: $p3
           middle: $middle
           cross1: $crossPoint1
           cross2: $crossPoint2
           """.stripMargin)
      }


      solve(index + 1)
    }

    solve(0)
  }

  def getMiddlePoint(pArray: Array[CustomPoint2D]): CustomPoint2D = {
    val c = pArray.foldLeft(CustomPoint2D(0, 0))((z, p) => CustomPoint2D(z.x + p.x, z.y + p.y))
    CustomPoint2D(c.x / pArray.length, c.y / pArray.length)
  }

  def findCrossPoint(l1: Line2D, l2: Line2D): CustomPoint2D = {
    val denom1 = l1.p2.x - l1.p1.x
    val denom2 = l2.p2.x - l2.p1.x
    val m1 = if (denom1 != 0) (l1.p2.y - l1.p1.y) / denom1 else Double.NaN
    val m2 = if (denom2 != 0) (l2.p2.y - l2.p1.y) / denom2 else Double.NaN

    if(denom1 == 0) {
      val b1 = l1.p1.x
      val b2 = l2.p1.y - m2 * l2.p1.x

      val crossX = b1
      val crossY = m2 * crossX + b2
      CustomPoint2D(crossX, crossY)
    } else if(denom2 == 0) {
      val b1 = l1.p1.y - m1 * l1.p1.x
      val b2 = l2.p1.x

      val crossX = b2
      val crossY = m1 * crossX + b1
      CustomPoint2D(crossX, crossY)
    } else {
      val b1 = l1.p1.y - m1 * l1.p1.x
      val b2 = l2.p1.y - m2 * l2.p1.x

      val crossX = (b2 - b1) / (m1 - m2)
      val crossY = m1 * crossX + b1
      CustomPoint2D(crossX, crossY)
    }
  }

  def checkConcavity(line: Line2D, crossPoint: CustomPoint2D): Boolean =
    isBetween(crossPoint.x, line.p1.x, line.p2.x) && isBetween(crossPoint.y, line.p1.y, line.p2.y)

  def isBetween(value: Double, a: Double, b: Double): Boolean =
    if (a > b) Math.max(b, value) == Math.min(value, a)
    else Math.max(a, value) == Math.min(value, b)

  def main(args: Array[String]): Unit = {
//        val a1 = Array(CustomPoint2D(-1, 3), CustomPoint2D(3, 1), CustomPoint2D(0, -1), CustomPoint2D(-2, 1))
//    customSolution(a1)

//    val a2 = Array(CustomPoint2D(-1, 3), CustomPoint2D(1, 2), CustomPoint2D(1, 1), CustomPoint2D(3, 1),
//      CustomPoint2D(0, -1), CustomPoint2D(-2, 1), CustomPoint2D(-1, 2))
//    customSolution(a2)
//
//    val a3 = Array(CustomPoint2D(-1, 3), CustomPoint2D(1, 2), CustomPoint2D(3, 1), CustomPoint2D(0, -1),
//      CustomPoint2D(-2, 1))
//    customSolution(a3)

    val boomerang = Array(CustomPoint2D(-1, 0), CustomPoint2D(1, 1), CustomPoint2D(0, 0), CustomPoint2D(1, -1))
    println(customSolution(boomerang))

  }

}


package code.playground.challanges.projectEuler

import code.playground.challanges.projectEuler.Utils._

object Problem5 {

  def solution(n: Int): Int = {
    val factorsAndRanks = scala.collection.mutable.Map[Int, Factor]()
    (2 to n).flatMap(getPrimeFactorsAndRanks).foreach(pf =>
      factorsAndRanks.get(pf.prime) match {
        case Some(value) => if (pf.rank > value.rank) factorsAndRanks.update(pf.prime, pf)
        case _ => factorsAndRanks.put(pf.prime, pf)
      })
    factorsAndRanks.foldLeft(1)((z, item) => z * Math.pow(item._2.prime, item._2.rank).toInt)
  }

  def main(args: Array[String]): Unit = {
    println(solution(20))
  }

}

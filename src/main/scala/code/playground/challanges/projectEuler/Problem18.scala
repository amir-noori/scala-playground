package code.playground.challanges.projectEuler

import code.playground.challanges.projectEuler.Utils._

object Problem18 {

  val TRIANGLE_STR =
    """
      |75
      |95 64
      |17 47 82
      |18 35 87 10
      |20 04 82 47 65
      |19 01 23 75 03 34
      |88 02 77 73 07 63 67
      |99 65 04 28 06 16 70 92
      |41 41 26 56 83 40 80 70 33
      |41 48 72 33 47 32 37 16 94 29
      |53 71 44 65 25 43 91 52 97 51 14
      |70 11 33 28 77 73 17 78 39 68 17 57
      |91 71 52 38 17 14 91 43 58 50 27 29 48
      |63 66 04 68 89 53 67 30 73 16 69 87 40 31
      |04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
      |""".stripMargin


  case class TGrid(rootNode: Option[TGridNode])

  case class TGridNode(level: Int, index: Int, value: Int, left: TGridNode, right: TGridNode)

  def solution(n: BigInt): Int = {
    val nodeValues: Seq[(Array[Int], Int)] =
      TRIANGLE_STR.strip().split("\r\n").toList
        .map(s => s.split(" ").map(s => Integer.parseInt(s)))
        .zipWithIndex


//    def generateGraph(values: Seq[(Array[Int], Int)], createdNodes: Seq[TGridNode]): Unit = {
//      values.head match {
//        case (currentValues, currentLevel) => currentValues.map()
//      }
//    }

    //    val nodeValues: Seq[Array[TGridNode]] = triangleValues
    //      .zipWithIndex
    //      .map({
    //        case (arr, i) => arr.zipWithIndex.map({
    //          case (value, j) => TGridNode(i, j, value, null, null)
    //        })
    //      })

    //    triangleValues.zipWithIndex.foldLeft(Graph(null))({ (g, x) =>
    //      g.rootNode match {
    //        case Some(rootNode) =>
    //        case None =>
    //      }
    //    })

    //    def generateGraph(nodeValues: Seq[Array[TGridNode]]): Unit = {
    //      nodeValues match {
    //        case h :: t =>
    //          if (t.nonEmpty) {
    //            h.foreach(node => node.copy())
    //          }
    //      }
    //    }

    println("")

    2
  }

  def main(args: Array[String]): Unit = {
    println(solution(1000))
  }

}

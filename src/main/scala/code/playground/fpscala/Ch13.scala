package code.playground.fpscala

object Ch13 {

  case class Player(name: String, score: Int)

  object Player {
    def printWinner(p: Player): Unit =
      println(p.name + " is the winner!")

    def declareWinner(p1: Player, p2: Player): Player =
      if (p1.score > p2.score) p1 else p2
  }


  trait IO {
    def run: Unit
  }

  def printLine(s: String) = new IO {
    override def run = println(s)
  }

  val message: IO = printLine("a message")

}

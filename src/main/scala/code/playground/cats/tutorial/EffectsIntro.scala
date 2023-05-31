package code.playground.cats.tutorial

import java.util.Calendar

//noinspection TypeAnnotation
object EffectsIntro {

  case class RIO[A](run: () => A) {
    def map[B](f: A => B): RIO[B] = RIO(() => f(run()))

    def flatMap[B](f: A => RIO[B]): RIO[B] = RIO(() => f(run()).run())
  }

  def main(args: Array[String]): Unit = {
    val currentTimeRIO = RIO[Long](() => Calendar.getInstance().get(Calendar.MILLISECOND))

    val computationRIO = RIO[Long](() => (1 to 1000000).foldLeft(1)(_ + _) - 1)

    val r = for {
      t1 <- currentTimeRIO
      result <- computationRIO
      t2 <- currentTimeRIO
    } yield (result, t2 - t1)

    val data = r.run()
    println(s"computation result: ${data._1}, time consumption: ${data._2}ms")

    val durationComputationRIO: RIO[(Long, Long)] = computationRIO match {
      case RIO(run) => RIO(() => {
        val t1 = Calendar.getInstance().get(Calendar.MILLISECOND)
        val result = run()
        val t2 = Calendar.getInstance().get(Calendar.MILLISECOND)
        (result, t2 - t1)
      })
    }
  }

}

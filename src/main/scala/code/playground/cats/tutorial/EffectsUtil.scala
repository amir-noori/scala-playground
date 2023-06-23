package code.playground.cats.tutorial

import cats.effect.IO

object EffectsUtil {

  def getThreadInfo: String = {
    val thread = Thread.currentThread()
    s"${thread.getName}[${thread.getId}]"
  }

  implicit class IODebugWrapper[A](io: IO[A]) {
    def debug: IO[A] = for {
      a <- io
      info = getThreadInfo
      _ = println(info)
    } yield a
  }

}

package code.playground.zio.scratch

trait ZIO[+A] {
  self =>

  def run(callback: A => Unit): Unit

  def zip[B](that: ZIO[B]): ZIO[(A, B)] = ZIO.Zip(self, that)

}

object ZIO {

  def succeed[A](value: => A): ZIO[A] = ZIO.Effect(() => value)

  def succeedNow[A](value: A): ZIO[A] = ZIO.Succeed(value)

  case class Succeed[A](value: A) extends ZIO[A] {
    override def run(callback: A => Unit): Unit = callback(value)
  }

  case class Effect[A](f: () => A) extends ZIO[A] {
    override def run(callback: A => Unit): Unit = callback(f())
  }

  case class Zip[A, B](left: ZIO[A], right: ZIO[B]) extends ZIO[(A, B)] {
    override def run(callback: ((A, B)) => Unit): Unit =
      left.run { l =>
        right.run { r =>
          callback(l, r)
        }
      }
  }
}

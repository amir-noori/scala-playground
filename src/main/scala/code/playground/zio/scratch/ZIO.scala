package code.playground.zio.scratch

trait ZIO[+A] { self =>

  import ZIO._

  def as[B](value: => B): ZIO[B] = self.map(_ => value)

  def run(callback: A => Unit): Unit

  def zip[B](that: ZIO[B]): ZIO[(A, B)] = ZIO.Zip(self, that)

  def map[B](f: A => B): ZIO[B] = ZIO.Map(self, f)

  def flatMap[B](f: A => ZIO[B]): ZIO[B] = ZIO.FlatMap(self, f)


  // this is not be stack safe
  def steps: Int = self match {
    case Succeed(_) => 1
    case Effect(_) => 1
    case zip: Zip[_, _] => zip.left.steps + zip.right.steps
  }

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
          callback((l, r))
        }
      }
  }

  case class Map[A, B](z: ZIO[A], f: A => B) extends ZIO[B] {
    override def run(callback: B => Unit): Unit =
      z.run { a => callback(f(a)) }
  }

  case class FlatMap[A, B](z: ZIO[A], f: A => ZIO[B]) extends ZIO[B] {
    override def run(callback: B => Unit): Unit =
      z.run { a => f(a).run(b => callback(b)) }
  }

}

package code.playground.zio.scratch.old

import scala.concurrent.ExecutionContext


trait Fiber[+A] {
  def start(): Unit

  def join: ZIO[A]
}

class FiberImpl[A](zio: ZIO[A]) extends Fiber[A] {
  var maybeResult: Option[A] = None
  var callbacks: List[A => Any] = List.empty[A => Any]

  override def start(): Unit =
    ExecutionContext.global.execute { () =>
      zio.run { a =>
        maybeResult = Some(a)
        callbacks.foreach(callback => callback(a))
      }
    }

  override def join: ZIO[A] = {
    maybeResult match {
      case Some(a) => ZIO.succeedNow(a)
      case None => ZIO.async { complete =>
        callbacks = complete :: callbacks
      }
    }
  }

}


// Declarative Encoding
// ZIO[R, E, A]
sealed trait ZIO[+A] {
  self =>

  def parZip[B](z: ZIO[B]): ZIO[(A, B)] =
    for {
      f1 <- self.fork
      f2 <- z.fork
      a <- f1.join
      b <- f2.join
    } yield (a, b)

  def fork: ZIO[Fiber[A]] = ZIO.Fork[A](self)

  def flatMap[B](f: A => ZIO[B]): ZIO[B] =
    ZIO.FlatMap(self, f)

  //  def map[B](f: A => B): ZIO[B] =
  //    ZIO.Map(self, f)

  def map[B](f: A => B): ZIO[B] =
    flatMap(a => ZIO.succeedNow(f(a)))

  //  def zip[B](that: ZIO[B]): ZIO[(A, B)] =
  //    ZIO.Zip(self, that)

  def zip[B](that: ZIO[B]): ZIO[(A, B)] =
    for {
      a <- self
      b <- that
    } yield (a, b)


  def run(callback: A => Unit): Unit
}


object ZIO {

  def async[A](register: (A => Any) => Any): ZIO[A] =
    ZIO.Async(register)

  def succeed[A](value: => A): ZIO[A] =
    ZIO.Effect(() => value)

  def succeedNow[A](value: A): ZIO[A] = ZIO.Succeed(value)

  case class Succeed[A](value: A) extends ZIO[A] {
    override def run(callback: A => Unit): Unit =
      callback(value)
  }

  case class Effect[A](f: () => A) extends ZIO[A] {
    override def run(callback: A => Unit): Unit =
      callback(f())
  }

  //  case class Zip[A, B](left: ZIO[A], right: ZIO[B]) extends ZIO[(A, B)] {
  //    override def run(callback: ((A, B)) => Unit): Unit =
  //      left.run(a => {
  //        right.run(b => {
  //          callback((a, b))
  //        })
  //      })
  //  }

  //  case class Map[A, B](z: ZIO[A], f: A => B) extends ZIO[B] {
  //    override def run(callback: B => Unit): Unit =
  //      z.run(a => callback(f(a)))
  //  }

  case class FlatMap[A, B](z: ZIO[A], f: A => ZIO[B]) extends ZIO[B] {
    override def run(callback: B => Unit): Unit =
      z.run(a => f(a).run(callback))
  }

  case class Async[A](register: (A => Any) => Any) extends ZIO[A] {
    override def run(callback: A => Unit): Unit = register(callback)
  }

  case class Fork[A](self: ZIO[A]) extends ZIO[Fiber[A]] {
    override def run(callback: Fiber[A] => Unit): Unit = {
      val fiber = new FiberImpl[A](self)
      fiber.start()
      callback(fiber)
    }
  }

}
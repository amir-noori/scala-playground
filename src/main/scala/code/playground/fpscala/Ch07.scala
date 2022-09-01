package code.playground.fpscala

import Ch07.SecondAttempt.Par

import java.util.concurrent.{ExecutorService, Executors, Future, TimeUnit}
//import scala.concurrent.Future


object Ch07 extends App {

  object FirstAttempt {

    trait Par[A] {
    }

    object Par {
      def unit[A](a: A): Par[A] = ???

      def run[A](a: Par[A]): A = ???

      def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???

      def fork[A](a: => Par[A]): Par[A] = ???

      def async[A](a: => A): Par[A] = fork(unit(a))
    }

    def sumWithParEnhanced(nums: IndexedSeq[Int]): Par[Int] = {
      if (nums.isEmpty) return Par.unit(0)
      if (nums.size == 1) Par.unit(nums.head)
      else {
        val (l, r) = nums.splitAt(nums.size / 2)
        Par.map2(sumWithParEnhanced(l), sumWithParEnhanced(r))(_ + _)
      }
    }

    def sumWithPar(nums: IndexedSeq[Int]): Int = {
      if (nums.isEmpty) return 0
      if (nums.size == 1) nums.head
      else {
        val (l, r) = nums.splitAt(nums.size / 2)
        val sumL: Par[Int] = Par.unit(sumWithPar(l))
        val sumR: Par[Int] = Par.unit(sumWithPar(r))
        Par.run(sumL) + Par.run(sumR)
      }
    }


    def sum(nums: IndexedSeq[Int]): Int = {
      if (nums.isEmpty) return 0
      if (nums.size == 1) nums.head
      else {
        val (l, r) = nums.splitAt(nums.size / 2)
        sum(l) + sum(r)
      }
    }

    val result = sum(IndexedSeq(10, 20, 30, 40, 50))

  }


  object SecondAttempt {

    type Par[A] = ExecutorService => Future[A]

    case class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {
      override def cancel(mayInterruptIfRunning: Boolean): Boolean =
        a.cancel(mayInterruptIfRunning) || b.cancel(mayInterruptIfRunning)

      override def isCancelled: Boolean = a.isCancelled || b.isCancelled

      override def isDone: Boolean = a.isDone && b.isDone

      override def get(): C = get(Long.MaxValue, TimeUnit.NANOSECONDS)

      override def get(timeout: Long, unit: TimeUnit): C = {
        // TODO: implement timeout
        f(a.get, b.get)
      }
    }

    object Par {
//      def unit[A](a: A): Par[A] = (es: ExecutorService) => {
//        println("33333333333333333333")
//        es.submit(() => {
//          println("4444444444444444444")
//          a
//        })
//      }

      def unit[A](a: A): Par[A] = (es: ExecutorService) => es.submit(() => a)

      def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

      def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

      def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
        es => {
          val fa = run(es)(a)
          val fb = run(es)(b)
          Map2Future(fa, fb, f)
        }
      }

      def map[A, B](a: Par[A])(f: A => B): Par[B] =
        map2(a, unit(()))((x, _) => f(x))

      //      def parMap[A, B](list: List[A])(f: A => B): Par[List[B]] =
      //        map(unit(list))(f)

      def sequence[A](listA: List[Par[A]]): Par[List[A]] =
        listA.foldRight(unit(List[A]()))((pa, acc) => map2(pa, acc)(_ :: _))

      def fork[A](a: => Par[A]): Par[A] = (es: ExecutorService) => es.submit(() => a(es).get)

      def async[A](a: => A): Par[A] = fork(unit(a))

      def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => unit(f(a))

      def sortedPar(list: Par[List[Int]]): Par[List[Int]] =
        map(list)(_.sorted)
    }


  }
}


object Test extends App {
  def sum(ints: IndexedSeq[Int]): Int = { // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
//    println(s"summing: thread name: ${Thread.currentThread().getName} and id: ${Thread.currentThread().getId}")
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l, r) = ints.splitAt(ints.length / 2) // Divide the sequence in half using the `splitAt` function.
      //      val result: Par[Int] = Par.map2[Int, Int, Int](Par.fork[Int](es => es.submit(() => sum(l))), Par.fork[Int](es => es.submit(() => sum(r))))((a: Int, b: Int) => a + b)

      //      val result2: Par[Int] = Par.map2(Par.unit(sum(l)), Par.unit(sum(r)))((a, b) => a + b)
//      val result2: Par[Int] = Par.map2(
//        Par.fork(es => {
//          println("111111111111111111")
//          es.submit(() => {
//            println("22222222222222222")
//            sum(l)
//          })
//        }),
//        Par.fork(Par.unit(sum(r)))
//      )((a, b) => a + b)

//      val result2: Par[Int] = Par.map2(
//        Par.fork(Par.unit(sum(l))),
//        Par.fork(Par.unit(sum(r)))
//      )((a, b) => a + b)

      val result2: Par[Int] = Par.map2(
        Par.fork(Par.unit(sum(l))),
        Par.fork(Par.unit(sum(r)))
      )((a, b) => a + b)

      val executorService = Executors.newFixedThreadPool(2);
      result2(executorService).get()
      //      result(executorService).get()
      // Recursively sum both halves and add the results together.
    }
  }


  def greet(name: String): Int = {
    Thread.sleep(Math.random().longValue()*10000)
    println(s"hi $name. This is current thread Info:")
    println(s"summing: thread name: ${Thread.currentThread().getName} and id: ${Thread.currentThread().getId}")
    0
  }

  val executorService = Executors.newFixedThreadPool(5)

  println("begin")
  val r1: Par[Int] = Par.async[Int](greet("me"))
  val r2: Par[Int] = Par.async[Int](greet("you"))
  val r3: Par[Int] = Par.async[Int](greet("them"))

  r1(executorService)
  r2(executorService)
  r3(executorService)

  println("end")

//  println("begin")
//  println(sum(IndexedSeq(
//    10, 20, 30, 40, 10, 20, 30, 40, 10, 20, 30, 40,
//    10, 20, 30, 40, 10, 20, 30, 40, 10, 20, 30, 40,
//    10, 20, 30, 40, 10, 20, 30, 40, 10, 20, 30, 40,
//    10, 20, 30, 40, 10, 20, 30, 40, 10, 20, 30, 40
//  )))
//  println("end")
}

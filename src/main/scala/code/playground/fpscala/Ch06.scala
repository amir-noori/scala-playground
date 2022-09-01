package code.playground.fpscala

import scala.annotation.tailrec

object Ch06 extends App {

  object RandomNumberGenerator {

    type Rand[+A] = RNG => (A, RNG)

    trait RNG {
      def nextInt: (Int, RNG)
    }

    object RNG {
      def simple(seed: Long): RNG = new RNG {
        def nextInt: (Int, RNG) = {
          val seed2 = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
          ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
        }
      }

      def unit[A](a: A): Rand[A] = rng => (a, rng)

      def flatMap[A, B](ra: Rand[A])(f: A => Rand[B]): Rand[B] =
        rng => {
          val (a, rng2) = ra(rng)
          f(a)(rng2)
        }

      def _map[A, B](r: Rand[A])(f: A => B): Rand[B] =
        flatMap(r)(x => unit(f(x)))

      def map[A, B](r: Rand[A])(f: A => B): Rand[B] =
        rng => {
          val (a, rng2) = r(rng)
          (f(a), rng2)
        }

      def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
        flatMap(ra) { a =>
          flatMap(rb) { b =>
            unit(f(a, b))
          }
        }
      }

      def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
        rng => {
          val (a, rng1) = ra(rng)
          val (b, rng2) = rb(rng1)
          (f(a, b), rng2)
        }
      }

      def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
        @tailrec
        def seqRec(fs: List[Rand[A]], acc: Rand[List[A]]): Rand[List[A]] = {
          fs match {
            case x :: xs => seqRec(xs, map2(x, acc)((a, la) => a :: la))
            case Nil => acc
          }
        }

        seqRec(fs, unit(List[A]()))
      }

      def positiveLessThan(n: Int): Rand[Int] =
        flatMap(positiveInt) { i =>
          val mod = i % n
          if (i + (n - 1) - mod > 0) unit(mod) else positiveLessThan(n)
        }

      def _sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
        fs.foldRight(unit(List[A]()))((a, acc) => map2(a, acc)(_ :: _))

      def positiveEven: Rand[Int] = {
        map(positiveIntRand)(x => if (x % 2 == 0) x else x + 1)
      }

      def positiveIntRand: Rand[Int] = {
        rng => {
          val (value, rng2) = rng.nextInt
          if (value >= 0) (value, rng2)
          else if (value == Int.MinValue) (Int.MaxValue, rng2)
          else (-value, rng2)
        }
      }

      def positiveInt(rng: RNG): (Int, RNG) = {
        val (value, rng2) = rng.nextInt
        if (value >= 0) (value, rng2)
        else if (value == Int.MinValue) (Int.MaxValue, rng2)
        else (-value, rng2)
      }

      def doubleRand: Rand[Double] = map(positiveIntRand)(_ / (Int.MaxValue + 1))

      def double(rng: RNG): (Double, RNG) = {
        val (value, rng2) = positiveInt(rng)
        (value / (Int.MaxValue + 1), rng2)
      }

      def intDoubleRand: Rand[(Int, Double)] = {
        map2(positiveIntRand, doubleRand)((_, _))
      }

      def intDouble(rng: RNG): ((Int, Double), RNG) = {
        val (i, rng2) = rng.nextInt
        val (d, rng3) = double(rng2)
        ((i, d), rng3)
      }

      def doubleInt(rng: RNG): ((Double, Int), RNG) = {
        val ((i, d), rng2) = intDouble(rng)
        ((d, i), rng2)
      }

      def double3(rng: RNG): ((Double, Double, Double), RNG) = {
        val (d1, rng1) = double(rng)
        val (d2, rng2) = double(rng1)
        val (d3, rng3) = double(rng2)
        ((d1, d2, d3), rng3)
      }

      def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

        @tailrec
        def intsRec(c: Int, r: RNG, acc: List[Int]): (List[Int], RNG) = {
          if (c > 0) {
            val (i, rng2) = rng.nextInt
            intsRec(count - 1, rng2, i :: acc)
          } else {
            (acc, r)
          }
        }

        intsRec(count, rng, List())
      }
    }


    def apply(): Unit = {
      val rng = RNG.simple(42)
      val (randomInt1, rng2) = rng.nextInt
      val (randomInt2, rng3) = rng2.nextInt
    }


  }

  RandomNumberGenerator()


  object StateMachine {

    import State._

    case class State[S, +A](run: S => (A, S)) {
      def map[B](f: A => B): State[S, B] =
        this.flatMap(a => unit(f(a)))

      def map2[B, C](state: State[S, B])(f: (A, B) => C): State[S, C] =
        this.flatMap(a =>
          state.map(b => f(a, b))
        )

      def flatMap[B](f: A => State[S, B]): State[S, B] = {
        State(s => {
          val (a, s1) = this.run(s)
          f(a).run(s1)
        })
      }
    }

    object State {
      def unit[S, A](a: A): State[S, A] = State(s => (a, s))

      def sequence[S, A](sList: List[State[S, A]]): State[S, List[A]] = {
        sList.foldRight(unit[S, List[A]](List[A]()))((x, acc) => x.map2(acc)(_ :: _))
      }
    }

    def apply(): Unit = {

    }
  }

  StateMachine()

}


object CandyMachineApp extends App {

  case class State[S, +A](run: S => (A, S)) {

    import State._

    def map[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => sb.map(b => f(a, b)))

    def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })

  }

  object State {

    def unit[S, A](a: A): State[S, A] =
      State(s => (a, s))

    // The idiomatic solution is expressed via foldRight
    def sequenceViaFoldRight[S,A](sas: List[State[S, A]]): State[S, List[A]] =
      sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

    // This implementation uses a loop internally and is the same recursion
    // pattern as a left fold. It is quite common with left folds to build
    // up a list in reverse order, then reverse it at the end.
    // (We could also use a collection.mutable.ListBuffer internally.)
    def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
      def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
        actions match {
          case Nil => (acc.reverse,s)
          case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
        }
      State((s: S) => go(s,sas,List()))
    }

    // We can also write the loop using a left fold. This is tail recursive like the
    // previous solution, but it reverses the list _before_ folding it instead of after.
    // You might think that this is slower than the `foldRight` solution since it
    // walks over the list twice, but it's actually faster! The `foldRight` solution
    // technically has to also walk the list twice, since it has to unravel the call
    // stack, not being tail recursive. And the call stack will be as tall as the list
    // is long.
    def sequenceViaFoldLeft[S,A](l: List[State[S, A]]): State[S, List[A]] =
      l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)( _ :: _ ))

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get // Gets the current state and assigns it to `s`.
      _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
    } yield ()

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  }

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  object Candy {

    import State._

    def update = (i: Input) => (s: Machine) =>
      (i, s) match {
        case (_, Machine(_, 0, _)) => s
        case (Coin, Machine(false, _, _)) => s
        case (Turn, Machine(true, _, _)) => s
        case (Coin, Machine(true, candy, coin)) =>
          Machine(false, candy, coin + 1)
        case (Turn, Machine(false, candy, coin)) =>
          Machine(true, candy - 1, coin)
      }

    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
      _ <- sequence(inputs map (modify[Machine] _ compose update))
      s <- get
    } yield (s.coins, s.candies)


    def simulateMachine_(inputs: List[Input]): State[Machine, (Int, Int)] = for {
      _ <- sequence(inputs.map(x => modify[Machine](update(x))))
      s <- get
    } yield (s.coins, s.candies)
  }

  val machine = Machine(locked = false, 150, 0)

  Candy.simulateMachine(List(Turn, Coin))

}

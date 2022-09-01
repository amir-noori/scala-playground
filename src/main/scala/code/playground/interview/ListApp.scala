package code.playground.interview

import scala.annotation.tailrec

object ListApp extends App {

  sealed abstract class RList[+A] {
    def head: A

    def tail: RList[A]

    def isEmpty: Boolean

    def ::[T >: A](element: T): RList[T] = new ::(element, this)

    def apply(index: Int): A

    def length: Int

    def reverse: RList[A]

    def ++[T >: A](newList: RList[T]): RList[T]

    def remove(index: Int): RList[A]

    def map[B](f: A => B): RList[B]

    def flatMap[B](f: A => RList[B]): RList[B]

    def filter(f: A => Boolean): RList[A]


    /**
     * run length encoding
     */
    def rle: RList[(A, Int)]

    def duplicateEach(n: Int): RList[A]

    def rotateLeft(n: Int): RList[A]

    def sample(n: Int): RList[A]


    /**
     * sorting methods
     */

    def insertionSorted[T >: A](ordering: Ordering[T]): RList[T]

    def mergeSorted[T >: A](ordering: Ordering[T]): RList[T]

  }

  case object RNil extends RList[Nothing] {
    override def head: Nothing = throw new NoSuchElementException

    override def tail: RList[Nothing] = throw new NoSuchElementException

    override def isEmpty: Boolean = true

    override def toString: String = "[]"

    def apply(index: Int): Nothing = throw new NoSuchElementException

    override def length: Int = 0

    override def reverse: RList[Nothing] = RNil

    override def ++[T >: Nothing](newList: RList[T]): RList[T] = newList

    override def remove(index: Int): RList[Nothing] = throw new NoSuchElementException

    override def map[B](f: Nothing => B): RList[B] = RNil

    override def flatMap[B](f: Nothing => RList[B]): RList[B] = RNil

    override def filter(f: Nothing => Boolean): RList[Nothing] = RNil

    /**
     * run length encoding
     */
    override def rle: RList[(Nothing, Int)] = RNil

    override def duplicateEach(n: Int): RList[Nothing] = RNil

    override def rotateLeft(n: Int): RList[Nothing] = RNil

    override def sample(n: Int): RList[Nothing] = RNil

    /**
     * sorting methods
     */
    override def insertionSorted[T >: Nothing](ordering: Ordering[T]): RList[T] = RNil

    override def mergeSorted[T >: Nothing](ordering: Ordering[T]): RList[T] = RNil
  }

  case class ::[+A](override val head: A, override val tail: RList[A]) extends RList[A] {
    override def isEmpty: Boolean = false

    override def toString: String = {
      @tailrec
      def toStringTailrec(list: RList[A], str: String): String = {
        if (list.isEmpty) return str
        if (list.tail.isEmpty) return s"$str ${list.head} "
        toStringTailrec(list.tail, s"$str ${list.head},")
      }

      "[" + toStringTailrec(this, "") + "]"
    }


    override def apply(index: Int): A = {
      @tailrec
      def applyTailrec(remaining: RList[A], currentIndex: Int): A = {
        if (currentIndex == index) remaining.head
        else applyTailrec(remaining.tail, currentIndex + 1)
      }

      if (index < 0) throw new NoSuchElementException
      else applyTailrec(this, 0)
    }

    override def length: Int = {
      @tailrec
      def findLengthTailrec(count: Int, list: RList[A]): Int = {
        if (list.isEmpty) return count
        findLengthTailrec(count + 1, list.tail)
      }

      findLengthTailrec(0, this)
    }

    override def reverse: RList[A] = {

      @tailrec
      def reverseTailrec(remainingList: RList[A], newList: RList[A]): RList[A] = {
        val acc = remainingList match {
          case RNil => newList
          case (x: A) :: RNil => x :: newList
          case (x: A) :: (_: RList[A]) => x :: newList
        }
        if (remainingList == RNil) {
          return acc
        }
        reverseTailrec(remainingList.tail, acc)
      }

      reverseTailrec(this.tail, this.head :: RNil)
    }

    def reverseImproved: RList[A] = {

      @tailrec
      def reverseTailrec(remainingList: RList[A], newList: RList[A]): RList[A] = {
        if (remainingList.isEmpty) return newList
        reverseTailrec(remainingList.tail, remainingList.head :: newList)
      }

      reverseTailrec(this, RNil)
    }

    // this implementation of concat will stackoverflow
    // override def ++[T >: A](newList: RList[T]): RList[T] = head :: (tail ++ newList)

    override def ++[T >: A](newList: RList[T]): RList[T] = {
      @tailrec
      def concatTailrec(newList: RList[T], resultList: RList[T]): RList[T] = {
        if (newList.isEmpty) return resultList
        concatTailrec(newList.tail, newList.head :: resultList)
      }

      concatTailrec(newList, this.reverse).reverse
    }

    override def remove(index: Int): RList[A] = {

      //      val listLength: Int = this.length
      //
      //      @tailrec
      //      def removeTailrec(actualList: RList[A], firstList: RList[A], secondList: RList[A], currentIndex: Int): RList[A] = {
      //        if (currentIndex == listLength) return firstList.reverse ++ secondList
      //        currentIndex match {
      //          case currentIndex if currentIndex < index => removeTailrec(actualList.tail, actualList.head :: firstList, secondList, currentIndex + 1)
      //          case currentIndex if currentIndex == index => removeTailrec(actualList.tail, firstList, secondList, currentIndex + 1)
      //          case currentIndex if currentIndex > index => removeTailrec(actualList.tail, firstList, actualList.head :: secondList, currentIndex + 1)
      //        }
      //      }
      //      removeTailrec(this, RNil, RNil, 0)

      @tailrec
      def removeTailrec(firstList: RList[A], secondList: RList[A], currentIndex: Int): RList[A] = {
        if (currentIndex == index) secondList.reverse ++ firstList.tail
        else if (firstList.isEmpty) secondList.reverse
        else removeTailrec(firstList.tail, firstList.head :: secondList, currentIndex + 1)
      }

      removeTailrec(this, RNil, 0)

    }

    override def map[B](f: A => B): RList[B] = {
      @tailrec
      def mapTailrec(list: RList[A], result: RList[B]): RList[B] = {
        if (list.isEmpty) return result.reverse
        mapTailrec(list.tail, f(list.head) :: result)
      }

      mapTailrec(this, RNil)
    }

    override def flatMap[B](f: A => RList[B]): RList[B] = {

      @tailrec
      def concatAllTailrec(list: RList[B], result: RList[B]): RList[B] = {
        if (list.isEmpty) return result
        concatAllTailrec(list.tail, list.head :: result)
      }

      @tailrec
      def flatMapTailrec(list: RList[RList[B]], result: RList[B]): RList[B] = {
        if (list.isEmpty) return result
        flatMapTailrec(list.tail, concatAllTailrec(list.head, result))
      }

      flatMapTailrec(this.map[RList[B]](x => f(x)), RNil).reverse

      //      @tailrec
      //      def flatMapTailrec(list: RList[A], result: RList[B]): RList[B] = {
      //        if (list.isEmpty) return result.reverse
      //        flatMapTailrec(list.tail, f(list.head).reverse ++ result)
      //      }
      //
      //      flatMapTailrec(this, RNil)
    }

    override def filter(f: A => Boolean): RList[A] = {
      @tailrec
      def filterTailrec(list: RList[A], result: RList[A]): RList[A] = {
        if (list.isEmpty) return result.reverse
        f(list.head) match {
          case true => filterTailrec(list.tail, list.head :: result)
          case false => filterTailrec(list.tail, result)
        }
      }

      filterTailrec(this, RNil)
    }

    /**
     * run length encoding
     */
    override def rle: RList[(A, Int)] = {

      @tailrec
      def rleRecursive(list: RList[A], lastElement: Option[A], count: Int, result: RList[(A, Int)]): RList[(A, Int)] = {

        list match {
          case x :: xs =>
            if (lastElement.isDefined) {
              if (x == lastElement.get) rleRecursive(list.tail, Some(x), count + 1, result)
              else rleRecursive(xs, Some(x), 1, (lastElement.get, count) :: result)
            } else {
              rleRecursive(xs, Some(x), 1, result)
            }
          case x :: RNil =>
            if (x == lastElement.get) rleRecursive(list.tail, lastElement, count + 1, result)
            else rleRecursive(RNil, Some(x), 1, (lastElement.get, count) :: result)
          case RNil => (lastElement.get, count) :: result
        }
      }

      rleRecursive(this, None, 1, RNil).reverse

    }

    override def duplicateEach(n: Int): RList[A] = {
      @tailrec
      def duplicateEachTailrec(originalList: RList[A], newList: RList[A], times: Int): RList[A] = {
        if (originalList.isEmpty) return newList.reverse
        if (n - times > 0) duplicateEachTailrec(originalList, originalList.head :: newList, times + 1)
        else duplicateEachTailrec(originalList.tail, originalList.head :: newList, 0)
      }

      duplicateEachTailrec(this, RNil, 0)
    }

    override def rotateLeft(n: Int): RList[A] = {
      if (n < 0) throw new IllegalArgumentException
      if (n == 0) return this
      if (n > this.length) return rotateLeft(n % this.length)

      @tailrec
      def getFirstNElements(list: RList[A], result: RList[A], remainingCount: Int): (RList[A], RList[A]) = {
        if (remainingCount > 0) getFirstNElements(list.tail, list.head :: result, remainingCount - 1)
        else (result, list)
      }

      val resultTuple: (RList[A], RList[A]) = getFirstNElements(this, RNil, n)
      resultTuple._2 ++ resultTuple._1.reverse
    }

    override def sample(n: Int): RList[A] = {
      if (n > this.length) throw new IllegalArgumentException

      @tailrec
      def sampleTailrec(list: RList[A], samples: RList[A], times: Int): RList[A] = {
        if (n - times == 0) return samples
        val index: Int = scala.util.Random.nextInt(list.length)
        val item: A = list(index)
        sampleTailrec(list.remove(index), item :: samples, times + 1)
      }

      sampleTailrec(this, RNil, 0)

    }

    /**
     * sorting methods
     */
    override def insertionSorted[T >: A](ordering: Ordering[T]): RList[T] = {

      @tailrec
      def insertSortTailrec(list: RList[T], sortedList: RList[T]): RList[T] = {
        if (list.isEmpty) sortedList
        else insertSortTailrec(list.tail, checkPredecessors(RNil, list.head, sortedList))
      }

      @tailrec
      def checkPredecessors(before: RList[T], item: T, after: RList[T]): RList[T] = {
        if (after.isEmpty || ordering.compare(item, after.head) > 0) before.reverse ++ (item :: after)
        else checkPredecessors(after.head :: before, item, after.tail)
      }

      //      @tailrec
      //      def checkPredecessors(item: T, orderedSubList: RList[T], result: RList[T]): RList[T] = {
      //        if (orderedSubList.isEmpty && result.isEmpty) return item :: result
      //        if (orderedSubList.isEmpty && !result.isEmpty) return result ++ (item :: RNil)
      //
      //        if (ordering.compare(item, orderedSubList.head) > 0)
      //          checkPredecessors(item, orderedSubList.tail, result ++ (orderedSubList.head :: RNil))
      //        else
      //          result ++ (item :: orderedSubList)
      //      }

      insertSortTailrec(this, RNil)
    }

    override def mergeSorted[T >: A](ordering: Ordering[T]): RList[T] = {

      /*
        Example:
          mergeSortTailrec([[3], [1], [2], [5], [4]], [])
          mergeSortTailrec([[2], [5], [4]], [[1, 3]])
          mergeSortTailrec([[4]], [[2, 5], [1, 3]])
          mergeSortTailrec([], [[4], [2, 5], [1, 3]])
          mergeSortTailrec([[4], [2, 5], [1, 3]], [])
          mergeSortTailrec([[1, 3]], [[2, 4 5]])
          mergeSortTailrec([], [[1, 3], [2, 4, 5]])
          mergeSortTailrec([[1, 3], [2, 4, 5]], [])
          mergeSortTailrec([], [[1, 2, 3, 4, 5]])
          [1, 2, 3, 4, 5]
      */

      def mergeSortTailrec(left: RList[RList[T]], right: RList[RList[T]]): RList[T] = {
        if (left.isEmpty) {
          if (right.tail.isEmpty) return right.head
          else if (right.isEmpty) return RNil
          else return mergeSortTailrec(right, left)
        }

        if (left.tail.isEmpty && right.isEmpty) return left.head
        if (left.tail.isEmpty && right.isEmpty) return left.head
        if (left.tail.isEmpty) mergeSortTailrec(RNil, mergeTailrec(left.head, RNil, RNil) :: right)
        else
          mergeSortTailrec(left.tail.tail, mergeTailrec(left.head, left.tail.head, RNil) :: right)
      }

      @tailrec
      def mergeTailrec(left: RList[T], right: RList[T], result: RList[T]): RList[T] = {
        if (left.isEmpty) return result.reverse ++ right
        if (right.isEmpty) return result.reverse ++ left
        if (ordering.compare(left.head, right.head) > 0)
          mergeTailrec(left, right.tail, right.head :: result)
        else
          mergeTailrec(left.tail, right, left.head :: result)
      }

      mergeSortTailrec(this.map(n => n :: RNil), RNil)


      //      @tailrec
      //      def breakTailrec(list: RList[T], result: RList[T]): (RList[T], RList[T]) = {
      //        if (list.length <= result.length) (list, result.reverse)
      //        else breakTailrec(list.tail, list.head :: result)
      //      }
      //
      //      def mergeSortTailrec(list: RList[T]): RList[T] = {
      //        if (list.length == 1) return list
      //        val brokenLists: (RList[T], RList[T]) = breakTailrec(list, RNil)
      //        val sortedList1: RList[T] = mergeSortTailrec(brokenLists._1)
      //        val sortedList2: RList[T] = mergeSortTailrec(brokenLists._2)
      //        mergeTailrec(sortedList1, sortedList2, RNil)
      //      }
      //
      //      @tailrec
      //      def mergeTailrec(left: RList[T], right: RList[T], result: RList[T]): RList[T] = {
      //        if (left.isEmpty) return result.reverse ++ right
      //        if (right.isEmpty) return result.reverse ++ left
      //        if (ordering.compare(left.head, right.head) > 0)
      //          mergeTailrec(left, right.tail, right.head :: result)
      //        else
      //          mergeTailrec(left.tail, right, left.head :: result)
      //      }
      //
      //      mergeSortTailrec(this)

    }
  }


  object RList {
    def from[A](iterable: Iterable[A]): RList[A] = {
      @tailrec
      def newRList(iterable2: Iterable[A], resultList: RList[A]): RList[A] = {
        if (iterable2.isEmpty) return resultList
        newRList(iterable2.tail, iterable2.head :: resultList)
      }

      newRList(iterable, RNil).reverse
    }
  }

  //  val x = Calendar.getInstance
  //  val y = Calendar.getInstance
  //  println(y.getTimeInMillis - x.getTimeInMillis)


  val list: RList[Int] = 10 :: 20 :: 30 :: 40 :: 40 :: 40 :: RNil
  //  println(list.reverse)
  //
  //  RList.from((1 to 10000).toList)
  //
  //  println(list ++ (1 :: 2 :: 3 :: RNil))

  //  println(list.remove(2))
  //  println(list.rle)
  //  println(list.duplicateEach(2))


  val list2: RList[Int] = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: RNil
  //  println(list2)
  //  for (i <- 1 to list2.length) {
  //    println(list2.rotateLeft(i))
  //  }

  //  println(list2.rotateLeft(7))
  //  println(list2.sample(3))

  val largeList: RList[Int] = RList.from((1 to 10000).toList)
  //  val x = Calendar.getInstance
  //  largeList.flatMap(x => (x + 10) :: RNil)
  //  val y = Calendar.getInstance
  //  println(y.getTimeInMillis - x.getTimeInMillis)

  val notOrderedList: RList[Int] = 10 :: 2 :: 44 :: 12 :: 5 :: 1 :: 3 :: RNil
  //  val notOrderedList: RList[Int] = 10 :: 20 :: 30 :: RNil
  println(notOrderedList.insertionSorted((x: Int, y: Int) => x - y))
  println(notOrderedList.mergeSorted((x: Int, y: Int) => x - y))


  val f = (data: String) => {
    println("f:" + data)
    "f"
  }
  val g = (data: String) => {
    println("g:" + data)
    "g"
  }

  val x = g compose f
  x("test")


//  val f = (x: Float) => x.abs
//  val g = (x: Float) => x + 3
//  val h1 = f compose g
//  val h2 = g compose f
}


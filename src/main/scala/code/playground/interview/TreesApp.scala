package code.playground.interview

import scala.annotation.tailrec


// Binary Tree
sealed abstract class BTree[+T] {
  def value: T

  def left: BTree[T]

  def right: BTree[T]

  def isEmpty: Boolean

  def isLeaf: Boolean

  def collectLeaves: List[BTree[T]]

  def leafCount: Int

  def size: Int

}

case object BEnd extends BTree[Nothing] {
  override def value: Nothing = throw new NoSuchElementException

  override def left: BTree[Nothing] = throw new NoSuchElementException

  override def right: BTree[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def isLeaf: Boolean = false

  override def collectLeaves: List[BTree[Nothing]] = throw new NoSuchElementException

  override def leafCount: Int = 0

  override val size: Int = 0
}

case class BNode[+T](override val value: T, override val left: BTree[T], override val right: BTree[T]) extends BTree[T] {
  override def isEmpty: Boolean = false

  override def isLeaf: Boolean = left.isEmpty && right.isEmpty

  override def collectLeaves: List[BTree[T]] = {

    @tailrec
    def collectTailRec(currentNode: BTree[T], leftAcc: List[BTree[T]], rightAcc: List[BTree[T]]): List[BTree[T]] = {
      if (currentNode.isLeaf) currentNode :: (leftAcc ++ rightAcc)
      else if (currentNode.isEmpty) leftAcc ++ rightAcc
      else if (currentNode.left.isLeaf) collectTailRec(currentNode.right, currentNode.left :: leftAcc, rightAcc)
      else if (currentNode.right.isLeaf) collectTailRec(currentNode.left, leftAcc, currentNode.right :: rightAcc)
      else collectTailRec(currentNode.right, leftAcc, rightAcc)
    }

    collectTailRec(this, List[BTree[T]](), List[BTree[T]]())
  }

  override def leafCount: Int = collectLeaves.size

  // because size is not a def but a val then the value is computed at the construction phase and this solution is not stack recursive
  override val size: Int = 1 + left.size + right.size
}


object TreesApp extends App {

  val tree: BTree[String] =
    BNode("grand father",
      BNode("son",
        BNode("grand son", BEnd, BEnd),
        BEnd),
      BNode("daughter", BEnd, BEnd))

  println(tree.size)

  val largeTree: BTree[Int] = (1 to 100000).foldLeft[BTree[Int]](BEnd) { (tree, i) =>
    BNode(i, tree, BEnd)
  }

  println(largeTree.size)

}

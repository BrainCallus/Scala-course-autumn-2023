package tree

import tree.TreeEntry.{Leaf, Node}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

case class AvlTree[T <: Comparable[T]](root: TreeEntry[T]) extends Tree[T] {

  override def add(element: T): AvlTree[T] = new AvlTree(AvlTree.add(element, root))

  override def delete(element: T): AvlTree[T] = new AvlTree(AvlTree.delete(element, root))

  override def breadthFirstSearch[E](func: (E, T) => E, accum: E): E = AvlTree.bfs(func, accum)(root)

  override def depthFirstSearch[E](func: (E, T) => E, accum: E): E = foldLeft(func, accum)

  def min(walker: ((T, T) => T, T) => T, minNeutral: T): T =
    walker((a, b) => if (a.compareTo(b) < 0) a else b, minNeutral)

  def max(walker: ((T, T) => T, T) => T, minNeutral: T): T =
    walker((a, b) => if (a.compareTo(b) > 0) a else b, minNeutral)

  def print(): Unit = {
    def f(seq: Seq[(Int, T)], value: T)(level: Int): Seq[(Int, T)] = seq.appended((level, value))
    val seq: Seq[(Int, T)] = AvlTree.abstractBfs(f, Seq())(Queue((root, 0)))
    seq.groupBy(elem => elem._1).foreach(entry => println(entry._2.mkString(" ")))
  }
}
object AvlTree {

  private def bfs[E, T](func: (E, T) => E, accum: E)(treeEntry: TreeEntry[T]): E =
    abstractBfs((x: E, y: T) => _ => func(x, y), accum)(Queue((treeEntry, 0)))

  @tailrec
  private def abstractBfs[E, T](func: (E, T) => Int => E, accum: E)(queue: Queue[(TreeEntry[T], Int)]): E =
    if (queue.isEmpty)
      accum
    else {
      val elementLast = queue.dequeue
      val entry = elementLast._1
      val nQueue = elementLast._2
      entry._1 match {
        case Leaf() => abstractBfs(func, accum)(nQueue)
        case Node(value, left, right, _) =>
          abstractBfs(func, func(accum, value)(entry._2))(
            nQueue.enqueue((left, entry._2 + 1)).enqueue((right, entry._2 + 1))
          )
      }
    }

  private def add[T <: Comparable[T]](element: T, node: TreeEntry[T]): TreeEntry[T] =
    node match {
      case Leaf() => Node(element, Leaf(), Leaf(), 1)
      case Node(value, left, right, _) =>
        element.compareTo(value) match {
          case 0  => node
          case -1 => balance(buildNode(value, add(element, left), right))
          case 1  => balance(buildNode(value, left, add(element, right)))
        }
    }

  private def delete[T <: Comparable[T]](element: T, node: TreeEntry[T]): TreeEntry[T] =
    node match {
      case Leaf() => Leaf()
      case Node(value, left, right, _) =>
        element.compareTo(value) match {
          case -1 => balance(buildNode(value, delete(element, left), right))
          case 1  => balance(buildNode(value, left, delete(element, right)))
          case 0  => balance(internalDelete(node))
        }
    }

  private def internalDelete[T](entry: TreeEntry[T]): TreeEntry[T] =
    entry match {
      case Node(_, Leaf(), Leaf(), _) => Leaf()
      case Node(_, left, Leaf(), _)   => left
      case Node(_, Leaf(), right, _)  => right
      case Node(v, left, right, _) =>
        def largestChild(node: TreeEntry[T]): (T, TreeEntry[T]) = node match {
          case Node(v, left, Leaf(), _) => (v, left)
          case Node(value, left, right, _) =>
            val rightRes = largestChild(right)
            (rightRes._1, buildNode(value, left, rightRes._2))
          case _ => (v, node)
        }
        val leftRes = largestChild(left)
        buildNode(leftRes._1, leftRes._2, right)
      case _ => entry

    }

  private def balance[T](node: TreeEntry[T]): TreeEntry[T] = getBalance(node) match {
    case -2 => handleLeftRotate(node)
    case 2  => handleRightRotate(node)
    case _  => node
  }

  private def handleLeftRotate[T](entry: TreeEntry[T]): TreeEntry[T] = entry match {
    case Node(_, _, right, _) =>
      if (getBalance(right) == 1) bigLeftRotate(entry)
      else leftRotate(entry)
    case _ => entry
  }

  private def handleRightRotate[T](entry: TreeEntry[T]): TreeEntry[T] =
    entry match {
      case Node(_, left, _, _) =>
        if (getBalance(left) == -1) bigRightRotate(entry)
        else rightRotate(entry)
      case _ => entry
    }

  private def bigLeftRotate[T](entry: TreeEntry[T]): TreeEntry[T] = entry match {
    case Node(value, left, right, _) => leftRotate(buildNode(value, left, rightRotate(right)))
    case _                           => entry
  }

  private def bigRightRotate[T](entry: TreeEntry[T]): TreeEntry[T] =
    entry match {
      case Node(value, left, right, _) => rightRotate(buildNode(value, leftRotate(left), right))
      case _                           => entry
    }

  private def leftRotate[T](entry: TreeEntry[T]): TreeEntry[T] = entry match {
    case Node(value, left, Node(vr, rl, rr, _), _) => buildNode(vr, buildNode(value, left, rl), rr)
    case _                                         => entry
  }

  private def rightRotate[T](entry: TreeEntry[T]): TreeEntry[T] = entry match {
    case Node(value, Node(vl, ll, lr, _), right, _) => buildNode(vl, ll, buildNode(value, lr, right))
    case _                                          => entry
  }

  private def buildNode[T](value: T, left: TreeEntry[T], right: TreeEntry[T]): Node[T] =
    Node(value, left, right, math.max(heightOf(left), heightOf(right)))

  private def heightOf(entry: TreeEntry[?]): Int = entry match {
    case Leaf()                => 0
    case Node(_, _, _, height) => height
  }

  private def getBalance(entry: TreeEntry[?]) = entry match {
    case Leaf()                  => 0
    case Node(_, left, right, _) => heightOf(left) - heightOf(right)
  }

}

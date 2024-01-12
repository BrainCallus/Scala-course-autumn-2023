package tree

import tree.TreeEntry
import tree.TreeEntry.{Leaf, Node}

trait Tree[T] {
  val root: TreeEntry[T]
  def add(element: T): Tree[T] // returns true if inserted successfully and false if specified element already exist
  def delete(element: T): Tree[T] // returns deleted element or neutral element if tree doesn't contain specified
  //  foldLeft:: (E -> T -> E) -> E -> Tree T -> E
  def foldLeft[E](func: (E, T) => E, accum: E): E = tFoldl(func, accum, root)
  def breadthFirstSearch[E](func: (E, T) => E, accum: E): E
  def depthFirstSearch[E](func: (E, T) => E, accum: E): E
  def size(): Int = foldLeft((a: Int, _) => a + 1, 0)

  private def tFoldl[E](func: (E, T) => E, accum: E, node: TreeEntry[T]): E =
    node match {
      case Leaf()                      => accum
      case Node(value, left, right, _) => tFoldl(func, func(tFoldl(func, accum, left), value), right)
    }
}

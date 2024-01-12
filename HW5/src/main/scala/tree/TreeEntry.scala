package tree

sealed trait TreeEntry[T]

object TreeEntry {
  case class Node[T](value: T, left: TreeEntry[T], right: TreeEntry[T], aux: Int = 0) extends TreeEntry[T] {}

  case class Leaf[T]() extends TreeEntry[T]
}

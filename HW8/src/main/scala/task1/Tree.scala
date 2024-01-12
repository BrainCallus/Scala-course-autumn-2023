package task1

sealed trait Tree[+T]

case class Branch[T](left: Tree[T], right: Tree[T]) extends Tree[T]

case class Leaf[+T](value: T) extends Tree[T]

object Tree {
  def getLeafsCount[T](tree: Tree[T]): Int = tFoldl((acc: Int, _: T) => acc + 1, 0, tree)
  private def tFoldl[T, E](func: (E, T) => E, accum: E, node: Tree[T]): E =
    node match {
      case Leaf(value)         => func(accum, value)
      case Branch(left, right) => tFoldl(func, tFoldl(func, accum, left), right)
    }
}

package tree

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tree.TreeEntry.Leaf

class AvlTreeSpec extends AnyFlatSpec with Matchers {

  val root1: AvlTree[Integer] = new AvlTree[Integer](Leaf()).add(10).add(1).add(-5).add(100).add(99).add(76).add(-200)

  def treeFind(element: Int, tree: AvlTree[Integer]): Boolean =
    tree.foldLeft((_: Boolean, elem) => elem == element, false)

  "add" should "add a distinct element into tree, i.e. if element already exist return same tree" in {
    val tree = root1.add(1000).add(1)
    assertResult(true)(treeFind(1000, tree))
    assertResult(false)(treeFind(1, tree.delete(1)))
  }

  "delete" should "remove specified element if it exist" in {
    val tree = root1.delete(10)
    assertResult(false)(treeFind(10, tree))
  }

  "foldLeft" should "accumulate result of function over the tree" in {
    assertResult(81)(root1.foldLeft((a: Int, b: Integer) => a + b, 0))
    assertResult(-127)(root1.foldLeft((a: Int, b: Integer) => b - a, 0))
    assertResult(-81)(root1.foldLeft((a: Int, b: Integer) => a - b, 0))
  }

  "min" should "find minimum in the tree using dfs or bfs" in {
    val dfsRes = root1.min(root1.depthFirstSearch, Int.MaxValue)
    val bfsRes = root1.min(root1.breadthFirstSearch, Int.MaxValue)
    assert(dfsRes == bfsRes)
    assertResult(-200)(dfsRes)
  }

  "max" should "find maximum in the tree using dfs or bfs" in {
    val dfsRes = root1.max(root1.depthFirstSearch, Int.MinValue)
    val bfsRes = root1.max(root1.breadthFirstSearch, Int.MinValue)
    assert(dfsRes == bfsRes)
    assertResult(100)(dfsRes)
  }

  "size" should "return amount of elements in the tree" in {
    assertResult(7)(root1.size())
    assertResult(7)(root1.add(1).add(100).size())
    assertResult(7)(root1.delete(-1000).add(1).size())
    assertResult(6)(root1.delete(-200).size())
    assertResult(8)(root1.add(5).size())
  }

  "print" should "print tree by levels" in {
    root1.print()
  }

}

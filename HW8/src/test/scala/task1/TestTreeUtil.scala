package task1

import scala.util.Random

object TestTreeUtil {
  sealed trait InvalidValue // maybe later kind of invalid String or something more interesting will be added

  case object InvalidInt extends InvalidValue

  implicit class TreeCheckerGenerator[T](generate: () => T) {

    def generateTree(limit: Int, probability: Double = 0.6): Tree[T] = internalGenerate(limit, probability, 0)._1

    private def internalGenerate(limit: Int, probability: Double, curSize: Int): (Tree[T], Int) = {
      if (curSize < limit && Random.nextFloat() <= probability) {
        val leftRes = internalGenerate(limit, probability, curSize)
        val right = internalGenerate(limit, probability, leftRes._2)
        (Branch(leftRes._1, right._1), right._2 + 1)
      } else (Leaf(generate()), curSize + 1)
    }
  }

  def mapChecker[T, B](initTree: Tree[T], resTree: Tree[B])(func: T => B): Boolean =
    (initTree, resTree) match {
      case (Leaf(value), Leaf(result)) => result == func(value)
      case (Branch(left, right), Branch(leftRes, rightRes)) =>
        mapChecker(left, leftRes)(func) && mapChecker(right, rightRes)(func)
      case _ => false
    }

  /** @param initTree
    *   [[Tree]] that was mapped
    * @param resTree
    *   initTree after ap with fincTree
    * @param funcTree
    *   tree with functions for map; if used not ap but map, funcTree = Leaf(function: T => B)
    * @tparam B
    *   result type
    * @return
    *   whether tree was (m)apped correct
    */
  def checkMapped[T, B](initTree: Tree[T], resTree: Tree[B])(funcTree: Tree[T => B]): Boolean =
    (funcTree, resTree) match {
      case (Leaf(func), x)                       => mapChecker(initTree, x)(func)
      case (Branch(lf, rf), Branch(left, right)) => checkMapped(initTree, left)(lf) && checkMapped(initTree, right)(rf)
      case _                                     => false
    }

  def checkFlatMap[T, B](initTree: Tree[T], resTree: Tree[B])(func: T => Tree[B]): Boolean =
    (initTree, resTree) match {
      case (Leaf(value), x) => x == func(value)
      case (Branch(left, right), Branch(flatL, flatR)) =>
        checkFlatMap(left, flatL)(func) && checkFlatMap(right, flatR)(func)
      case _ => false
    }

  // funcTree got from f: T=>Tree[B] since f(Leaf(x)) = ap(ft: Tree[T=>B])(Leaf(x))
  // EX: let f = \x->Branch(Leaf(f1(x)), Branch(Leaf(f2(x)), Leaf(f3(x)))) in flatMap(Leaf(v))(f)
  // ~ ap(fTree)(Leaf(v)) where fTree = Branch(Leaf(\x -> f1(x)), Branch(Leaf(\x -> f2(x)), Leaf(\x -> f3(x))))
  // convert T=>Tree[B] to Tree[T=>B] by hand
  def checkFlatMapByAp[T, B](initTree: Tree[T], resTree: Tree[B])(funcTree: Tree[T => B]): Boolean = {
    (initTree, resTree) match {
      case (Leaf(value), x) => checkMapped(Leaf(value), x)(funcTree)
      case (Branch(left, right), Branch(flatL, flatR)) =>
        checkFlatMapByAp(left, flatL)(funcTree) && checkFlatMapByAp(right, flatR)(funcTree)
      case _ => false
    }
  }
}

package task1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import task1.hierarchy.TypeClasses._

import scala.util.Random
import task1.TestTreeUtil._
import task1.hierarchy.TypeClasses.TreeFunctor.map
import task1.hierarchy.TypeClasses.TreeApplicative.pure
import task1.hierarchy.TypeClasses.TreeApply.ap
import task1.hierarchy.TypeClasses.TreeFlatMap.flatMap

class TypeClassesSpec extends AnyFlatSpec with Matchers {
  val DEFAULT_TEST_SIZE = 100

  val modelIntegerTree: Tree[Int] =
    Branch(
      Branch(
        Leaf(10),
        Branch(
          Branch(Leaf(3), Leaf(-100)),
          Leaf(0)
        )
      ),
      Branch(
        Branch(Leaf(7), Leaf(3)),
        Branch(Leaf(1), Leaf(-1))
      )
    )

  val modelStringTree: Tree[String] =
    Branch(
      Branch(Leaf("15"), Leaf("lol")),
      Branch(
        Branch(Leaf("0"), Leaf("170324")),
        Leaf("kek")
      )
    )
  type IntOp = Int => Int
  type IntToIntOp = Int => IntOp // I'm lazy

  val partOps: List[IntToIntOp] =
    List(
      (x: Int) => _ * x,
      (x: Int) => _ + x,
      (x: Int) => _ - x,
      (x: Int) => x - _,
      (x: Int) => _ / x
    )

  def countLeafs[T]: Tree[T] => Int = Tree.getLeafsCount

  implicit class Compose[A, B](val fn: A => B) {
    def >>[C](f2: B => C): A => C = (x: A) => f2(fn(x))
  }

  def randomSign: Int = if (Random.nextFloat() < 0.5) -1 else 1

  def randomIntFunction: IntOp =
    partOps(Random.nextInt(partOps.length))(Random.nextInt(100000) match {
      case 0 => -1 // escape DBZ
      case x => x * randomSign
    })

  def generateRandIntTree(sizeConstraint: Int, numberConstraint: Int = 2 ^ 15): Tree[Int] =
    (() => Random.nextInt(numberConstraint) * randomSign) generateTree sizeConstraint

  def generateIntToIntFuncTree(constraint: Int): Tree[IntOp] =
    (() => {
      randomIntFunction
    }) generateTree constraint

  def funcTreeToFlatMapMapper[T, B](funcTree: Tree[T => B]): T => Tree[B] =
    (x: T) => ap(funcTree)(Leaf(x))

  "getLeafsCount" should "return amount of Leafs in Tree" in {
    assertResult(8)(Tree.getLeafsCount(modelIntegerTree))
    assertResult(5)(Tree.getLeafsCount(modelStringTree))
  }

  "map" should "map values in leafs with given function" in {
    assertResult(modelIntegerTree)(map(modelIntegerTree)(identity))
    assertResult(
      Branch(
        Branch(Leaf(19), Branch(Branch(Leaf(5), Leaf(-201)), Leaf(-1))),
        Branch(Branch(Leaf(13), Leaf(5)), Branch(Leaf(1), Leaf(-3)))
      )
    )(
      map(modelIntegerTree)(partOps.head(2) >> partOps(2)(1)) // 2x-1
    )
    assertResult(
      Branch(
        Branch(Leaf("10"), Branch(Branch(Leaf("3"), Leaf("-100")), Leaf("0"))),
        Branch(Branch(Leaf("7"), Leaf("3")), Branch(Leaf("1"), Leaf("-1")))
      )
    )(
      map(modelIntegerTree)((x: Int) => x.toString)
    )

    val toOpt = map(modelStringTree)(x => x.toIntOption)
    assertResult(
      Branch(Branch(Leaf(Some(15)), Leaf(None)), Branch(Branch(Leaf(Some(0)), Leaf(Some(170324))), Leaf(None)))
    )(toOpt)
    val toInt: Option[Int] => Either[InvalidValue, Int] = {
      case None        => Left(InvalidInt)
      case Some(value) => Right(value)
    }
    assertResult(
      Branch(
        Branch(Leaf(Right(15)), Leaf(Left(InvalidInt))),
        Branch(Branch(Leaf(Right(0)), Leaf(Right(170324))), Leaf(Left(InvalidInt)))
      )
    )(map(toOpt)(toInt))
  }

  "map kind of stress" should "not fail" in {
    Range inclusive (1, DEFAULT_TEST_SIZE) foreach (_ => {
      val intTree = generateRandIntTree(DEFAULT_TEST_SIZE)
      val f = randomIntFunction >> ((x: Int) => x.toString)
      assert(mapChecker(intTree, map(intTree)(f))(f))
    })
  }

  "ap" should "return tree form given trees FT and T with number of leafs equal to leafs(FT)*leafs(T) and correct values" in {
    Range inclusive (1, 10) foreach (_ => {
      val intTree = generateRandIntTree(10)
      val funcTree = generateIntToIntFuncTree(10)
      val appedTree = ap(funcTree)(intTree)
      assertResult(countLeafs(intTree) * countLeafs(funcTree))(countLeafs(appedTree))
      assert(checkMapped(intTree, appedTree)(funcTree))
    })
  }

  "pure" should "wrap value into Leaf" in {
    Range(0, DEFAULT_TEST_SIZE) foreach (_ => {
      val genStr: String = Random.nextString(Random.nextInt(10))
      assert(pure(genStr) match {
        case Leaf(value) => value == genStr
        case _           => false
      })

      val divOpt = (x: Int, y: Int) => if (y == 0) None else Some(x / y)
      val x = Random.nextInt(10000)
      val y = Random.nextInt(3)
      assert(pure(divOpt(x, y)) match {
        case Leaf(value) =>
          value match {
            case None      => y == 0
            case Some(res) => res == x / y
          }
        case _ => false
      })
    })
  }

  "flatMap" should "be correct" in {
    Range(0, DEFAULT_TEST_SIZE) foreach (_ => {
      val funcTree = generateIntToIntFuncTree(10)
      val mapper = funcTreeToFlatMapMapper(funcTree)
      val intTree = generateRandIntTree(DEFAULT_TEST_SIZE)
      val fMapped = flatMap(intTree)(mapper)
      assert(checkFlatMap(intTree, fMapped)(mapper))
      assert(checkFlatMapByAp(intTree, fMapped)(funcTree))

      val funcEitherTree: Tree[Int => Either[InvalidValue, Int]] = (() => {
        val x = Random.nextInt(100000) * randomSign
        (y: Int) => if (x <= 0 || y == 0) Left(InvalidInt) else Right(x / y ^ 2)
      }) generateTree (20, 0.3)
      val eitherMapper = funcTreeToFlatMapMapper(funcEitherTree)
      val intTree2 = generateRandIntTree(DEFAULT_TEST_SIZE, DEFAULT_TEST_SIZE)
      val eitherMapped = flatMap(intTree2)(eitherMapper)
      assert(checkFlatMap(intTree2, eitherMapped)(eitherMapper))
      assert(checkFlatMapByAp(intTree2, eitherMapped)(funcEitherTree))
    })
  }

  "map" should "produce same results for TreeFunctor, TreeApply, TreeApplicative, TreeFlatMap and TreeMonad" in {
    Range(0, DEFAULT_TEST_SIZE) foreach (_ => {
      val func = randomIntFunction
      val intTree = generateRandIntTree(DEFAULT_TEST_SIZE)
      val functorRes = TreeFunctor.map(intTree)(func)
      val applyRes = TreeApply.map(intTree)(func)
      val applicativeRes = TreeApplicative.map(intTree)(func)
      val flatMapRes = TreeFlatMap.map(intTree)(func)
      val monadRes = TreeMonad.map(intTree)(func)

      assertResult(functorRes)(applyRes)
      assertResult(applyRes)(applicativeRes)
      assertResult(applicativeRes)(flatMapRes)
      assertResult(flatMapRes)(monadRes)
    })
  }

  "ap" should "produce same results for TreeApply, TreeApplicative, TreeFlatMap and TreeMonad" in {
    Range(0, DEFAULT_TEST_SIZE) foreach (_ => {
      val funcTree = generateIntToIntFuncTree(DEFAULT_TEST_SIZE)
      val intTree = generateRandIntTree(DEFAULT_TEST_SIZE)
      val applyRes = TreeApply.ap(funcTree)(intTree)
      val applicativeRes = TreeApplicative.ap(funcTree)(intTree)
      val flatMapRes = TreeFlatMap.ap(funcTree)(intTree)
      val monadRes = TreeMonad.ap(funcTree)(intTree)

      assertResult(applyRes)(applicativeRes)
      assertResult(applicativeRes)(flatMapRes)
      assertResult(flatMapRes)(monadRes)
    })
  }

  "pure" should "produce same results for TreeApplicative and TreeMonad" in {
    Range(0, DEFAULT_TEST_SIZE) foreach (_ => {
      val str = Random.nextString(Random.nextInt(1000000) + 1)
      val applicativeRes = TreeApplicative.pure(str)
      val monadRes = TreeMonad.pure(str)

      assertResult(applicativeRes)(monadRes)
    })
  }

  "flatMap" should "produce same results for TreeFlatMap and TreeMonad" in {
    Range(0, DEFAULT_TEST_SIZE) foreach (_ => {
      val mapper = funcTreeToFlatMapMapper(generateIntToIntFuncTree(DEFAULT_TEST_SIZE))
      val intTree = generateRandIntTree(DEFAULT_TEST_SIZE)
      val flatMapRes = TreeFlatMap.flatMap(intTree)(mapper)
      val monadRes = TreeMonad.flatMap(intTree)(mapper)

      assertResult(flatMapRes)(monadRes)
    })
  }
}

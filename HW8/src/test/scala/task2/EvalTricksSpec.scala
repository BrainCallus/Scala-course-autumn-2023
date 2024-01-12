package task2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import task2.EvalTricks._

import scala.language.postfixOps
import scala.io.Source
import scala.util.{Random, Using}

class EvalTricksSpec extends AnyFlatSpec with Matchers {

  // fibonacci from HW2 that work correct
  "fib" should "results should correspond to fibonacci results" in {
    assertResult(fibonacci(0))(fib(0))
    assertResult(fibonacci(1))(fib(1))
    fibonacci(1)
    Range(0, 1000) foreach (_ => {
      val n = Random.nextInt(10000) + 1
      assertResult(fibonacci(n))(fib(n))
    })
  }

  "fib" should "not fail on large numbers" in {
    Using(Source.fromFile("answer/ans.txt")) { source =>
      {
        val ans: BigInt = BigInt(source.getLines().next())
        assertResult(ans)(fib(1000000))
      }
    }

  }

  "foldRight" should "correct fold list" in {
    val list: List[Int] = List(1, 2, -10, 9, 25, 100, -50)
    assertResult(77)(foldRight(list, 0)(_ + _))
    assertResult(87)(foldRight(list, 10)(_ + _))
    assertResult(-145)(foldRight(list, 0)(_ - _))
    assertResult(-144)(foldRight(list, -1)(_ - _))
  }

  "foldRight" should "not fail on large list" in {
    val list: List[Int] = (0 until Int.MaxValue / 500 map { _ => 1 }).toList
    assertResult(Int.MaxValue / 500)(foldRight(list, 0)(_ + _))
  }
}

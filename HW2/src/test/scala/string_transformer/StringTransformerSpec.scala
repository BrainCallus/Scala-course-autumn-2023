package string_transformer

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import string_transformer.StringTransformer._

import scala.util.Random

class StringTransformerSpec extends AnyFlatSpec with Matchers {
  val DEFAULT_TEST_SIZE = 100

  "duplicate" should "duplicate string" in {
    assertResult("")(duplicate(""))
    assertResult("abab")(duplicate("ab"))
    assertResult("abobabob")(duplicate("abob"))
    assertResult("0289a0289a")(duplicateConcat("0289a"))
  }
  "duplicateConcat" should "duplicate string" in {
    assertResult("")(duplicateConcat(""))
    assertResult("abab")(duplicateConcat("ab"))
    assertResult("abobabob")(duplicateConcat("abob"))
    assertResult("0289a0289a")(duplicateConcat("0289a"))
  }

  "cutHalf" should "returns a part of string from 0 to it's length/2" in {
    assertResult("abcd")(cutHalf("abcdefgh"))
    assertResult("abc")(cutHalf("abcdefg"))
    assertResult("")(cutHalf(""))
  }

  def reverseMethodTest(function: String => String): Unit = {
    for (_ <- 0 until DEFAULT_TEST_SIZE) {
      val generatedSource = Random.nextString(Random.nextInt(100))
      assertResult(generatedSource.reverse)(function(generatedSource))

    }
  }

  "reverseNaive" should "return reversed string" in {
    reverseMethodTest(reverseNaive)
  }
  "reverseTail" should "return reversed string" in {
    reverseMethodTest(reverseTail)
  }
  "reverseActual" should "return reversed string" in {
    reverseMethodTest(reverseActual)
  }

  "transform" should "apply given function to string" in {
    assertResult("12341234")(transform(duplicate)("1234"))
    assertResult("abcd")(transform(cutHalf)("abcdefgh"))
    assertResult("abc")(transform(cutHalf)("abcdefg"))
    assertResult("")(transform(cutHalf)(""))
    reverseMethodTest(transform(reverseNaive))
    reverseMethodTest(transform(reverseTail))
    reverseMethodTest(transform(reverseActual))
  }

  "transform" should "apply composite functions" in {
    assertResult("12341234")(transform(duplicate _ compose cutHalf)("123456789"))
    assertResult("123456789")(transform(cutHalf _ compose duplicate)("123456789"))
    assertResult("cbacba")(transform(duplicate _ compose reverseActual)("abc"))
    assertResult("cbacba")(transform(reverseActual _ compose duplicate)("abc"))
    assertResult("kek")(transform(cutHalf _ compose reverseActual)("lolkek"))
    assertResult("lol")(transform(reverseActual _ compose cutHalf)("lolkek"))
    assertResult("22022022")(
      transform(
        cutHalf _ compose transform(
          reverseNaive _ compose transform(duplicateConcat _ compose cutHalf) compose duplicate
        )
      )("22022022")
    )
  }

}

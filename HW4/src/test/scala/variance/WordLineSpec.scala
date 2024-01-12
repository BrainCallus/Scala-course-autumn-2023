package variance

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class WordLineSpec extends AnyFlatSpec with Matchers {
  "WordLine constructor" should "accept given line" in {
    val line = WordLine("12345")
    assertResult("12345")(line.word)
    val line2 = WordLine("ad3\n!#\"sadf\r\nklm49")
    assertResult("ad3\n!#\"sadf\r\nklm49")(line2.word)
  }

  "WordLine constructor" should "accept given empty line" in {
    val line = WordLine("")
    assertResult("")(line.word)
  }

  "WordLine constructor" should "throw LineIsNullException if given line is null" in {
    assertThrows[LineIsNullException] {
      WordLine(null)
    }
  }

  "RedactedWordLine constructor" should "accept given redactionFactor and line" in {
    val redactedLine = RedactedWordLine(0.55, "abracadabra")
    assertResult(0.55)(redactedLine.redactionFactor)
    assertResult("abracadabra")(redactedLine.word)

    val redactedLine2 = RedactedWordLine(0, "123sdf")
    assertResult(0.0)(redactedLine2.redactionFactor)
    assertResult("123sdf")(redactedLine2.word)

    val redactedLine3 = RedactedWordLine(1, "abracadabra")
    assertResult(1)(redactedLine3.redactionFactor)
    assertResult("abracadabra")(redactedLine3.word)
  }

  "RedactedWordLine constructor" should "throw LineIsNullException if given line is null" in {
    assertThrows[LineIsNullException] {
      RedactedWordLine(0, null)
    }
  }

  "RedactedWordLine constructor" should "throw LineIsNullException if given redactionFactor out of [0; 1]" in {
    Array
      .fill(50) { -Random.nextDouble() }
      .foreach(prob => {
        assertThrows[ProbabilityIllegalValueException] {
          RedactedWordLine(prob, "word")
        }
      })
  }

}

package variance

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ConverterSpec extends AnyFlatSpec with Matchers {
  "LineConverter" should "return immutable WordLine word + \\n" in {
    assertResult("239\nabcd!@\n")(LineConverter.convert(new WordLine("239\nabcd!@")))
    assertResult("  \n\n")(LineConverter.convert(new WordLine("  \n")))
    assertResult("\n")(LineConverter.convert(new WordLine("")))
  }

  "LineConverter" should "return immutable RedactedWordLine word + \\n" in {
    assertResult("239\nabcd!@\n")(LineConverter.convert(new RedactedWordLine(1, "239\nabcd!@")))
    assertResult("  \n\n")(LineConverter.convert(new RedactedWordLine(1, "  \n")))
    assertResult("\n")(LineConverter.convert(new RedactedWordLine(1, "")))
  }

  "RedactedLineConverter" should "return world or sequence of '█'+\n with RedactedLine word length + 1" in {
    assertResult("██████████\n")(RedactedLineConverter.convert(new RedactedWordLine(1, "239\nabcd!@")))
    assertResult("\n")(RedactedLineConverter.convert(new RedactedWordLine(1, "")))
    assertResult("abcd\n")(RedactedLineConverter.convert(new RedactedWordLine(0, "abcd")))
    assertResult(11)(RedactedLineConverter.convert(new RedactedWordLine(0.85, "1234567890")).length)
  }

}

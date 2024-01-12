package variance

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ProjectorSpec extends AnyFlatSpec with Matchers {

  "Projector[WordLine]" should "project Slide[WordLine] with Converter[WordLine]" in {
    val projector = new Projector[WordLine](LineConverter)
    val projected = projector.project(
      new HelloSlide[WordLine](
        Seq(new WordLine("abc"), new WordLine("1,23"))
      )
    )
    assertResult("abc\n1,23\n")(projected)
  }

  "Projector[WordLine]" should " project Slide[RedactedWordLine] with Converter[WordLine]" in {
    val projector = new Projector[WordLine](LineConverter)
    val projected = projector.project(
      new HelloSlide[RedactedWordLine](
        Seq(
          new RedactedWordLine(0.4, " hello "),
          new RedactedWordLine(0.001, "hell "),
          new RedactedWordLine(0.99, "  !")
        )
      )
    )
    assertResult(" hello \nhell \n  !\n")(projected)
  }

  "Projector[WordLine]" should "not project anything with Converter[RedactedWordLine] " in {
    assertDoesNotCompile("val projector = new Projector[WordLine](RedactedLineConverter)")
  }

  "Projector[RedactedWordLine]" should "project Slide[RedactedWordLine] with Converter[WordLine]" in {
    val projector: Projector[RedactedWordLine] = new Projector[RedactedWordLine](LineConverter)

    val projected = projector.project(
      new HelloSlide[RedactedWordLine](
        Seq(new RedactedWordLine(0.25, "abc"), new RedactedWordLine(0.3, "1,23"))
      )
    )
    assertResult("abc\n1,23\n")(projected)
  }

  "Projector[RedactedWordLine]" should " project Slide[RedactedWordLine] with Converter[RedactedWordLine]" in {
    val projector = new Projector[RedactedWordLine](RedactedLineConverter)
    val projected = projector.project(
      new HelloSlide[RedactedWordLine](
        Seq(
          new RedactedWordLine(0.25, "cat"),
          new RedactedWordLine(0.3, "    12398239 "),
          new RedactedWordLine(0.8, "I'm to lazy to come up with normal examples of the lines.."),
          new RedactedWordLine(0.46, "Sorry")
        )
      )
    )
    println(projected)
  }

  "Projector[RedactedWordLine]" should "not project anything with Slide[WordLine] " in {

    assertDoesNotCompile(
      "new Projector[RedactedWordLine](LineConverter).project(new HelloSlide[RedactedWordLine]( Seq(new WordLine(\"abc\"), new WordLine(\"1,23\"))))"
    )

    assertDoesNotCompile(
      "new Projector[RedactedWordLine](RedactedLineConverter).project(new HelloSlide[RedactedWordLine]( Seq(new WordLine(\"abc\"), new WordLine(\"1,23\"))))"
    )

  }

}

package variance

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SlideSpec extends AnyFlatSpec with Matchers {

  "HelloSlide constructor" should "accept given WordLine sequence" in {
    val lines = Seq("abcd", "10ds", "русский текст")
    val lineIterator = lines.iterator
    val wordLines = lines map WordLine.apply
    val helloSlide = new HelloSlide[WordLine](wordLines)
    helloSlide.lines map (line => line.word) foreach (word => {
      assertResult(lineIterator.next())(word)
    })
  }

  "HelloSlide constructor" should "accept given RedactedWordLine sequence" in {
    val lines = Seq((0.333, "abcd"), (0.7812, "10ds"), (0.9126, "русский текст"))
    val lineIterator = lines.iterator
    val wordLines = lines map (tuple => new RedactedWordLine(tuple._1, tuple._2))
    val helloSlide = new HelloSlide[RedactedWordLine](wordLines)
    helloSlide.lines map (line => (line.redactionFactor, line.word)) foreach (tuple => {
      val initialTuple = lineIterator.next()
      assertResult(initialTuple._1)(tuple._1)
      assertResult(initialTuple._2)(tuple._2)
    })
  }

  "HelloSlide constructor" should "accept Nil" in {
    val slideWL = new HelloSlide[WordLine](Nil)
    assertResult(Nil)(slideWL.lines)
    val slideRWL = new HelloSlide[RedactedWordLine](Nil)
    assertResult(Nil)(slideRWL.lines)
  }

  "read" should "return pair of option of first Seq[WordLine] element and new slide with initial sequence tail" in {
    val lines = Seq("abcd", "10ds", "русский текст")
    val lineIterator = lines.iterator
    val wordLines = lines map WordLine.apply
    val slide = new HelloSlide[WordLine](wordLines)
    val readed = slide.read
    assertResult(lineIterator.next())(readed._1.get.word)
    readed._2.lines map (line => line.word) foreach (word => {
      assertResult(lineIterator.next())(word)
    })
  }

  "read" should "return pair of option of first Seq[RedactedWordLine] element and new slide with initial sequence tail" in {
    val lines = Seq((0.333, "abcd"), (0.7812, "10ds"), (0.9126, "русский текст"))
    val lineIterator = lines.iterator
    val wordLines = lines map (tuple => new RedactedWordLine(tuple._1, tuple._2))
    val slide = new HelloSlide[RedactedWordLine](wordLines)
    val readed = slide.read
    val fstFact_fstWord = lineIterator.next()
    assertResult(fstFact_fstWord._1)(readed._1.get.redactionFactor)
    assertResult(fstFact_fstWord._2)(readed._1.get.word)
    readed._2.lines map (line => (line.redactionFactor, line.word)) foreach (tuple => {
      val initialTuple = lineIterator.next()
      assertResult(initialTuple._1)(tuple._1)
      assertResult(initialTuple._2)(tuple._2)
    })
  }

  "read" should "return (None, new HelloSlide(Nill)) in given line sequence is Nil" in {
    val slide = new HelloSlide[WordLine](Nil)
    val res = slide.read
    assertResult(None)(res._1)
    assertResult(Nil)(res._2.lines)
  }

}

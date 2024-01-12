package variance

import scala.util.Random

trait Converter[-S] {
  def convert(value: S): String
}

object LineConverter extends Converter[WordLine] {
  override def convert(value: WordLine): String = value.word + "\n"
}

object RedactedLineConverter extends Converter[RedactedWordLine] {
  override def convert(value: RedactedWordLine): String =
    (if (Random.nextDouble() < value.redactionFactor) "█".repeat(value.word.length) else value.word) + "\n"
}

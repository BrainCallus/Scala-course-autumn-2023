package variance

class WordLine(val word: String)

object WordLine {
  def apply(word: String): WordLine = {
    if (word == null)
      throw LineIsNullException()
    new WordLine(word)
  }
}

class RedactedWordLine(val redactionFactor: Double, word: String) extends WordLine(word) {}

object RedactedWordLine {
  def apply(redactionFactor: Double, word: String): RedactedWordLine = {
    if (redactionFactor < 0 || redactionFactor > 1.0)
      throw ProbabilityIllegalValueException()
    if (word == null)
      throw LineIsNullException()
    new RedactedWordLine(redactionFactor, word)
  }
}

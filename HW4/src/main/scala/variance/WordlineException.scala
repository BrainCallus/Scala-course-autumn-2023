package variance

sealed abstract class WordlineException(message: String) extends IllegalArgumentException(message)

case class LineIsNullException() extends WordlineException(message = "Line is null!")
case class ProbabilityIllegalValueException()
  extends WordlineException(message = "Probability must be between 0.0 and 1.0!")

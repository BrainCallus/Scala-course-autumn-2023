package object utils {
  implicit class Ternary[T](condition: Boolean) {
    def ??(onTrue: => T, onFalse: => T): T = if (condition) onTrue else onFalse
  }

  final case class PositiveInt(value: Int)

  object PositiveInt {
    def from(int: Int): Option[PositiveInt] = int match {
      case x if x > 0 => Some(new PositiveInt(x))
      case _          => None
    }
  }

}

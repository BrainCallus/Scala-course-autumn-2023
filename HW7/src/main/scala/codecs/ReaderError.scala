package codecs

import scala.annotation.unused
sealed abstract class ReaderError(@unused message: String, @unused field: String)
case class WrongType(field: String, message: String = "Wrong field type") extends ReaderError(message, field)
case class AbsentField(field: String, message: String = "Absent field") extends ReaderError(message, field)

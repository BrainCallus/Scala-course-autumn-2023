package resident

import exceptions.BuildingError
import exceptions.BuildingError.AgeError
import utils.PositiveInt

sealed trait Gender

case object Female extends Gender

case object Male extends Gender

class Resident private (val gender: Gender, val age: PositiveInt) {
  def isMan: Boolean = gender == Male
}

object Resident {
  def apply(gender: Gender, age: Int): Either[BuildingError, Resident] = {
    PositiveInt.from(age) match {
      case Some(value) => Right(new Resident(gender, value))
      case None        => Left(AgeError)
    }
  }
}

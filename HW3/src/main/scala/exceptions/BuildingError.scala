package exceptions

class BuildingError(message: String)

object BuildingError {
  case class IllegalCommercialCount(message: String) extends BuildingError(message)

  case object AgeError extends BuildingError(message = "Can't resolve negative age")

  case object BuildingAddressError extends BuildingError(message = "Building should have non empty address")

  case object BuildingFloorError extends BuildingError(message = "Attic can't be a first floor of the building")
}

package floor

import resident.Resident
import exceptions.BuildingError
import exceptions.BuildingError.IllegalCommercialCount
import utils.PositiveInt

trait Floor // rewrite me

case class Attic(shops: Option[PositiveInt]) extends Floor
object Attic {
  def apply(shops: Int = 0): Either[IllegalCommercialCount, Attic] =
    shops match {
      case 0 | 1 => Right(new Attic(PositiveInt.from(shops)))
      case x if PositiveInt.from(x).isEmpty =>
        Left(IllegalCommercialCount("Floors can't have negative amount of the shops"))

      case _ => Left(IllegalCommercialCount("Attic can have no more then 1 shop"))
    }
}
case class Commercial(shops: PositiveInt, up: Floor) extends Floor

object Commercial {
  def apply(shops: Int, up: Floor): Either[BuildingError, Commercial] =
    PositiveInt.from(shops) match {
      case Some(int) => Right(new Commercial(int, up))
      case None      => Left(IllegalCommercialCount("Commercial floor must have at least 1 shop"))
    }
}

case class ResidentialFloor(residents: (Resident, Resident), up: Floor) extends Floor {
  def getResidents: (Resident, Resident) = residents
}

object ResidentialFloor {
  def apply(firstResident: Resident, secondResident: Resident, up: Floor): ResidentialFloor =
    new ResidentialFloor((firstResident, secondResident), up)
}

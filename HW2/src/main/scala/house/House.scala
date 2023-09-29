package house

import house_exception._

import scala.annotation.unused

case class House private (private val houseType: HouseType, floors: Long, length: Long, width: Long, height: Long) {
  @unused
  def reconParquetCost(): Long = houseType.parquetCostFunction(floors)(length, width, height)

  def getHouseType: String = houseType.toString
}

object House {
  def apply(houseType: String, floors: Long, length: Long, width: Long, height: Long): House = {
    new House(
      getHouseType(houseType),
      testPositive(floors, FloorsShouldBePositiveException),
      testPositive(length, LengthShouldBePositiveException),
      testPositive(width, WidthShouldBePositiveException),
      testPositive(height, HeightShouldBePositiveException)
    )
  }

  // just for sake of pattern matching
  private def getHouseType(name: String): HouseType = name.toLowerCase match {
    case "economy" | "premium" => HouseType.withName(name.toLowerCase.capitalize)
    case _                     => throw new NoSuchHouseTypeException(name)
  }

  private def testPositive(dimension: Long, kind: DimensionShouldBePositiveException): Long =
    if (dimension > 0) dimension else throw kind

}

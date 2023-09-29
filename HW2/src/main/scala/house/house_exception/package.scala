package house

package object house_exception {

  class NoSuchHouseTypeException(val illegalType: String) extends IllegalArgumentException(s"No such type $illegalType")

  abstract class DimensionShouldBePositiveException(dimension: String)
    extends IllegalArgumentException(s"$dimension should be positive")

  case object FloorsShouldBePositiveException extends DimensionShouldBePositiveException("Floors")

  case object LengthShouldBePositiveException extends DimensionShouldBePositiveException("Length")

  case object WidthShouldBePositiveException extends DimensionShouldBePositiveException("Width")

  case object HeightShouldBePositiveException extends DimensionShouldBePositiveException("Height")

}

package house
import enumeratum._

sealed abstract class HouseType(val name: String, val parquetCostFunction: (Long => (Long, Long, Long) => Long))
  extends EnumEntry

object HouseType extends Enum[HouseType] {
  private def sum3: (Long, Long, Long) => Long = _ + _ + _
  case object Premium
    extends HouseType(
      "premium",
      floors =>
        (length, width, height) =>
          ((x: Long) => (x ^ floors) * sum3(length, width, height)).apply(floors match {
            case y if y < 5 => 3
            case _          => 2
          })
    )

  case object Economy
    extends HouseType("economy", floors => (length, width, height) => 10000 * floors + length * width * height)

  val values: IndexedSeq[HouseType] = findValues
}

package building

import resident.Resident
import exceptions.BuildingError
import exceptions.BuildingError._
import floor._
import utils._
import utils.PositiveInt
import resident._

import scala.annotation.{tailrec, unused}

/** Здание должно иметь:
  *   - строковый адрес
  *   - этажи (сходящиеся к первому этажу) Этаж может быть жилым, коммерческим, либо чердаком (который сам может быть
  *     коммерческим). На каждом жилом этаже живет 2 человека и есть лестница(ссылка) ведущая на следующий этаж У
  *     каждого человека есть возраст (>0) и пол На коммерческом этаже может быть несколько заведений (используйте
  *     Array), но не меньше 1. Здание всегда должно заканчиваться чердаком На чердаке никто не живет, но это может быть
  *     и коммерческое помещение (но только 1).
  */

case class Building private (address: String, floor: Floor) {}

object Building {

  def apply(address: String, floor: Floor): Either[BuildingError, Building] =
    if (address == null || address.trim.isEmpty) {
      Left(BuildingAddressError)
    } else
      floor match {
        case _: Attic => Left(BuildingFloorError)
        case _        => Right(new Building(address, floor))
      }

  /** Проходится по зданию снизу в вверх, применяя функцию [[f]] на каждом жилом этаже с начальным аккумулятором
    * [[accumulator]]
    */
  def fold(building: Building, accumulator: Int)(f: (Int, Floor) => Int, step: Int = 1): Int = {
    foldHelper(Some(building.floor), accumulator)(f, step)
  }
  @tailrec
  private def foldHelper(floor: Option[Floor], accum: Int)(f: (Int, Floor) => Int, step: Int): Int = {
    if (floor.isEmpty) accum
    else {
      val someFloor = floor.get
      someFloor match {
        case _: ResidentialFloor => foldHelper(walkUp(someFloor, step), f(accum, someFloor))(f, step)
        case _: Commercial       => foldHelper(walkUp(someFloor, step), f(accum, someFloor))(f, step)
        case x: Attic            => f(accum, x)
        case _                   => accum
      }
    }
  }

  /** не знаю, насколько в текущем контексте стоит заморачиваться с наличием функции, высчитывающей значение tuple._2, и
    * возвратом пары переменных
    */
  def fold2(building: Building, accumulator: Int)(f: (Int, Floor) => Int, step: Int = 1): Double = {
    val tupleDiv = (x: (Int, Int)) => x._1 / x._2.doubleValue
    tupleDiv(foldHelper2(Some(building.floor), accumulator)(f, step))
  }

  @tailrec
  private def foldHelper2(floor: Option[Floor], accum: Int, visited: Int = 1)(
    f: (Int, Floor) => Int,
    step: Int
  ): (Int, Int) = {
    if (floor.isEmpty) (accum, visited)
    else {
      val someFloor = floor.get
      someFloor match {
        case _: ResidentialFloor => foldHelper2(walkUp(someFloor, step), f(accum, someFloor), visited + 1)(f, step)
        case _: Commercial       => foldHelper2(walkUp(someFloor, step), f(accum, someFloor), visited + 1)(f, step)
        case x: Attic            => (f(accum, x), visited)
        case _                   => (accum, visited)
      }
    }
  }

  private def byFloorTypeFunction(
    residentialFunc: (Int, ResidentialFloor) => Int,
    commercialFunc: (Int, Commercial) => Int,
    atticFunc: (Int, Attic) => Int
  )(int: Int, floor: Floor) =
    floor match {
      case residential: ResidentialFloor => residentialFunc(int, residential)
      case commercial: Commercial        => commercialFunc(int, commercial)
      case attic: Attic                  => atticFunc(int, attic)
    }

  private def getPerFloor[F <: Floor, E](
    floor: F,
    propertiesGetter: F => E,
    predicate: E => Boolean
  )(mapper: E => Int, default: Int = 0): Int = {
    def getProperty(floorProperty: E, cond: E => Boolean): Option[Int] = {
      cond(floorProperty) ?? (Some(mapper(floorProperty)), None)
    }
    propertiesGetter.andThen(getProperty(_, predicate).getOrElse(default))(floor)
  }

  /** Подсчитывает количество этажей, на которых живет хотя бы один мужчина старше [[olderThan]]. Используйте [[fold]]
    */
  def countOldManFloors(building: Building, olderThan: Int): Int =
    fold(building, 0)(
      byFloorTypeFunction(
        (acc, res) => {
          val isManOlderThen = (resident: Resident) => { resident.gender == Male && resident.age.value > olderThan }
          getPerFloor(
            res,
            (f: ResidentialFloor) => f.getResidents,
            (residents: (Resident, Resident)) => isManOlderThen(residents._1) || isManOlderThen(residents._2)
          )((_: (Resident, Resident)) => 1) + acc
        },
        getAccumIgnoreFloor,
        getAccumIgnoreFloor
      )
    )

  /** Находит наибольший возраст женщины, проживающей в здании. Используйте [[fold]] */
  def womanMaxAge(building: Building): Int = fold(building, 0)(
    byFloorTypeFunction(
      (acc, floor) => {
        def getAfeIfWoman(resident: Resident): Int = (!resident.isMan) ?? (resident.age.value, 0)
        getPerFloor(
          floor,
          (f: ResidentialFloor) => f.getResidents,
          (residents: (Resident, Resident)) => !residents._1.isMan || !residents._2.isMan
        )(
          (residents: (Resident, Resident)) => getAfeIfWoman(residents._1) max getAfeIfWoman(residents._2),
          acc
        ) max acc
      },
      getAccumIgnoreFloor,
      getAccumIgnoreFloor
    )
  )

  /** Находит кол-во коммерческих заведений в здании. Используйте [[fold]] */
  def countCommercial(building: Building): Int = fold(building, 0)(
    byFloorTypeFunction(
      getAccumIgnoreFloor,
      (acc, floor) => {
        getPerFloor(floor, (f: Commercial) => f.shops, (_: PositiveInt) => true)((x: PositiveInt) => x.value) + acc
      },
      (acc, attic) => acc + (attic.shops.isEmpty ?? (0, 1))
    )
  )

  /** Находит среднее кол-во коммерческих заведений в зданиях. Реализуйте свою функцию, похожую на [[fold]] для прохода
    * по зданию
    */
  def countCommercialAvg(buildings: Array[Building]): Double =
    (buildings map countCommercial).sum / (buildings.isEmpty ?? (1.0, buildings.length.doubleValue))

  /** Находит среднее кол-во мужчин на четных этажах. Реализуйте свою функцию, похожую на [[fold]] для прохода по зданию
    */
  def evenFloorsMenAvg(building: Building): Double = {
    def compute(floor: Floor): Double = fold2(new Building(building.address, floor), 0)(
      byFloorTypeFunction(
        (acc, res) => {
          getPerFloor(
            res,
            (f: ResidentialFloor) => f.getResidents,
            (residents: (Resident, Resident)) => residents._1.isMan || residents._2.isMan
          )((residents: (Resident, Resident)) => (residents._1.isMan && residents._2.isMan) ?? (2, 1)) + acc
        },
        getAccumIgnoreFloor,
        getAccumIgnoreFloor
      ),
      2
    )
    val firstEven = walkUp(building.floor, 1)
    firstEven.getOrElse(Attic) match {
      case _: Attic            => 0.0
      case x: ResidentialFloor => compute(x)
      case x: Commercial       => compute(x)
    }
  }

  @tailrec
  def walkUp(floor: Floor, levels: Int): Option[Floor] =
    floor match {
      case r: ResidentialFloor => if (levels == 0) Some(floor) else walkUp(r.up, levels - 1)
      case c: Commercial       => if (levels == 0) Some(floor) else walkUp(c.up, levels - 1)
      case _: Attic            => if (levels == 0) Some(floor) else None
    }

  private def getAccumIgnoreFloor(x: Int, @unused y: Floor): Int = x

}

package building

import resident._
import exceptions.BuildingError._
import floor._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.PositiveInt

class BuildingSpec extends AnyFlatSpec with Matchers {
  def getBuilding(address: String, floor: Floor): Building = Building(address, floor).getOrElse(null)

  def getAttic(shops: Int): Attic = Attic(shops).getOrElse(null)

  def getCommercial(shops: Int, floor: Floor): Commercial = Commercial(shops, floor).getOrElse(null)

  def getResident(gender: Gender, age: Int): Resident = Resident(gender, age).getOrElse(null)

  "building constructor" should "create building with given address and floor" in {
    val building = getBuilding("no_fantasy", getCommercial(2, getAttic(1)))
    assertResult("no_fantasy")(building.address)
    val floor = building.floor
    floor match {
      case x: Commercial =>
        assert(x.shops == PositiveInt(2))
        x.up match {
          case _: Attic => assert(true)
          case _        => assert(false)
        }
      case _ => assert(false)
    }
  }

  it should "return buildingAddressError if address is null or empty" in {
    val build1 = Building(null, getCommercial(2, getAttic(1)))
    build1 match {
      case Left(BuildingAddressError) =>
      case _                          => assert(false)
    }
    val build2 = Building("      ", getCommercial(2, getAttic(1)))
    build2 match {
      case Left(BuildingAddressError) => assert(true)
      case _                          => assert(false)
    }
  }

  it should "return BuildingFloorError if given floor is Attic" in {
    val building = Building("addr", getAttic(1))
    building match {
      case Left(BuildingFloorError) => assert(true)
      case _                        => assert(false)
    }
  }

  "countOldManFloors" should "return the number of men older than the specified age" in {
    val firstFloor =
      ResidentialFloor(
        getResident(Female, 2),
        getResident(Female, 67),
        ResidentialFloor(
          getResident(Male, 34),
          getResident(Female, 29),
          getCommercial(
            3,
            ResidentialFloor(
              getResident(Male, 11),
              getResident(Male, 1),
              ResidentialFloor(getResident(Female, 45), getResident(Male, 55), getAttic(1))
            )
          )
        )
      )

    assertResult(3)(Building.countOldManFloors(getBuilding("address", firstFloor), 10))
    assertResult(1)(Building.countOldManFloors(getBuilding("address", firstFloor), 34))
  }

  it should "return 0 if there are no men older than the specified age" in {
    val firstFloor =
      ResidentialFloor(
        getResident(Female, 2),
        getResident(Female, 67),
        ResidentialFloor(
          getResident(Male, 34),
          getResident(Female, 29),
          getCommercial(
            3,
            ResidentialFloor(
              getResident(Male, 11),
              getResident(Male, 1),
              ResidentialFloor(getResident(Female, 45), getResident(Male, 55), getAttic(1))
            )
          )
        )
      )

    assertResult(0)(Building.countOldManFloors(getBuilding("address", firstFloor), 55))
  }

  it should "return 0 if there are no men at all in the building" in {
    val firstFloor =
      ResidentialFloor(
        getResident(Female, 2),
        getResident(Female, 67),
        ResidentialFloor(
          getResident(Female, 34),
          getResident(Female, 29),
          getCommercial(
            3,
            ResidentialFloor(
              getResident(Female, 11),
              getResident(Female, 1),
              ResidentialFloor(getResident(Female, 45), getResident(Female, 55), getAttic(1))
            )
          )
        )
      )

    assertResult(0)(Building.countOldManFloors(getBuilding("no_man", firstFloor), 55))

  }

  "womanMaxAge" should "find age of the oldest woman in the building" in {
    val firstFloor =
      ResidentialFloor(
        getResident(Female, 2),
        getResident(Female, 67),
        ResidentialFloor(
          getResident(Female, 78),
          getResident(Female, 78),
          getCommercial(
            3,
            ResidentialFloor(
              getResident(Female, 11),
              getResident(Female, 1),
              ResidentialFloor(
                getResident(Female, 45),
                getResident(Female, 55),
                getCommercial(
                  1,
                  getCommercial(
                    2,
                    ResidentialFloor(
                      getResident(Female, 34),
                      getResident(Female, 29),
                      getCommercial(43, ResidentialFloor(getResident(Male, 46), getResident(Female, 24), getAttic(0)))
                    )
                  )
                )
              )
            )
          )
        )
      )

    assertResult(78)(Building.womanMaxAge(getBuilding("address", firstFloor)))
    assertResult(55)(Building.womanMaxAge(getBuilding("address", Building.walkUp(firstFloor, 3).get)))
  }

  it should "return 0 if there are no women in the building" in {
    val firstFloor =
      ResidentialFloor(
        getResident(Male, 2),
        getResident(Male, 67),
        ResidentialFloor(getResident(Male, 78), getResident(Male, 78), getAttic(0))
      )
    assertResult(0)(Building.womanMaxAge(getBuilding("no_woman", firstFloor)))
  }

  "countCommercial" should "return number of commercial establishments in the building" in {
    val firstFloor =
      ResidentialFloor(
        getResident(Female, 2),
        getResident(Female, 67),
        ResidentialFloor(
          getResident(Female, 78),
          getResident(Female, 78),
          getCommercial(
            3,
            ResidentialFloor(
              getResident(Female, 11),
              getResident(Female, 1),
              ResidentialFloor(
                getResident(Female, 45),
                getResident(Female, 55),
                getCommercial(
                  1,
                  getCommercial(
                    2,
                    ResidentialFloor(
                      getResident(Female, 34),
                      getResident(Female, 29),
                      getCommercial(43, ResidentialFloor(getResident(Male, 46), getResident(Female, 24), getAttic(0)))
                    )
                  )
                )
              )
            )
          )
        )
      )
    val firstFloor2 =
      ResidentialFloor(
        getResident(Female, 2),
        getResident(Female, 67),
        ResidentialFloor(
          getResident(Female, 78),
          getResident(Female, 78),
          getCommercial(
            3,
            ResidentialFloor(
              getResident(Female, 11),
              getResident(Female, 1),
              ResidentialFloor(
                getResident(Female, 45),
                getResident(Female, 55),
                getCommercial(
                  1,
                  getCommercial(
                    2,
                    ResidentialFloor(
                      getResident(Female, 34),
                      getResident(Female, 29),
                      getCommercial(43, ResidentialFloor(getResident(Male, 46), getResident(Female, 24), getAttic(1)))
                    )
                  )
                )
              )
            )
          )
        )
      )
    assertResult(49)(Building.countCommercial(getBuilding("address", firstFloor)))
    assertResult(50)(Building.countCommercial(getBuilding("address", firstFloor2)))

  }

  it should "return 0 if there are no commercial establishments in the building" in {
    val firstFloor =
      ResidentialFloor(
        getResident(Female, 2),
        getResident(Female, 67),
        ResidentialFloor(
          getResident(Female, 78),
          getResident(Female, 78),
          ResidentialFloor(
            getResident(Female, 11),
            getResident(Female, 1),
            ResidentialFloor(
              getResident(Female, 45),
              getResident(Female, 55),
              ResidentialFloor(
                getResident(Female, 34),
                getResident(Female, 29),
                ResidentialFloor(getResident(Male, 46), getResident(Female, 24), getAttic(0))
              )
            )
          )
        )
      )
    assertResult(0)(Building.countCommercial(getBuilding("no_commercial", firstFloor)))
  }

  "countCommercialAvg" should "return average amount of shops per 1 building for several buildings" in {
    val firstFloor =
      ResidentialFloor(
        getResident(Female, 2),
        getResident(Female, 67),
        ResidentialFloor(
          getResident(Female, 78),
          getResident(Female, 78),
          getCommercial(
            3,
            ResidentialFloor(
              getResident(Female, 11),
              getResident(Female, 1),
              ResidentialFloor(
                getResident(Female, 45),
                getResident(Female, 55),
                getCommercial(
                  2,
                  getCommercial(
                    2,
                    ResidentialFloor(
                      getResident(Female, 34),
                      getResident(Female, 29),
                      getCommercial(3, ResidentialFloor(getResident(Male, 46), getResident(Female, 24), getAttic(1)))
                    )
                  )
                )
              )
            )
          )
        )
      )
    val additionalFloor =
      getCommercial(5, getCommercial(1, ResidentialFloor(getResident(Male, 87), getResident(Male, 33), getAttic(0))))
    val buildingA = getBuilding("address1", firstFloor)
    val buildingB = getBuilding("address2", Building.walkUp(firstFloor, 7).get)
    val buildingC = getBuilding("address3", additionalFloor)
    assertResult(7)(Building.countCommercialAvg(Array(buildingA, buildingB, buildingC)))
    val buildingD = getBuilding("address4", Building.walkUp(firstFloor, 3).get)
    assertResult(7.25)(Building.countCommercialAvg(Array(buildingA, buildingB, buildingC, buildingD)))
  }

  it should "be 0 if no commercials in buildings" in {
    val firstFloor =
      ResidentialFloor(
        getResident(Female, 2),
        getResident(Female, 67),
        ResidentialFloor(
          getResident(Female, 78),
          getResident(Female, 78),
          ResidentialFloor(getResident(Male, 5), getResident(Female, 46), getAttic(0))
        )
      )
    val additionalFloor = ResidentialFloor(getResident(Female, 9), getResident(Male, 33), getAttic(0))
    val buildingA = getBuilding("address1", firstFloor)
    val buildingB = getBuilding("address2", Building.walkUp(firstFloor, 2).get)
    val buildingC = getBuilding("address3", additionalFloor)
    assertResult(0)(Building.countCommercialAvg(Array(buildingA, buildingB, buildingC)))
  }

  it should "be 0 if given array of buildings is empty" in {
    assertResult(0)(Building.countCommercialAvg(Array.empty))
  }

  "evenFloorsMenAvg" should "find average amount of man on even floor" in {
    val firstFloor =
      ResidentialFloor(
        getResident(Male, 2),
        getResident(Female, 67),
        ResidentialFloor(
          getResident(Male, 78),
          getResident(Male, 78),
          ResidentialFloor(
            getResident(Female, 11),
            getResident(Male, 1),
            ResidentialFloor(
              getResident(Male, 45),
              getResident(Female, 55),
              ResidentialFloor(
                getResident(Female, 34),
                getResident(Female, 29),
                ResidentialFloor(
                  getResident(Female, 46),
                  getResident(Male, 24),
                  ResidentialFloor(
                    getResident(Male, 2),
                    getResident(Female, 3),
                    getCommercial(
                      3,
                      ResidentialFloor(
                        getResident(Female, 3),
                        getResident(Female, 89),
                        getCommercial(2, getCommercial(4, getAttic(0)))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    assertResult(4 / 6.doubleValue)(Building.evenFloorsMenAvg(getBuilding("address", firstFloor)))

  }

  it should "be 0 if no men in building" in {
    val firstFloor =
      ResidentialFloor(
        getResident(Female, 2),
        getResident(Female, 67),
        ResidentialFloor(
          getResident(Female, 34),
          getResident(Female, 29),
          getCommercial(
            3,
            ResidentialFloor(
              getResident(Female, 11),
              getResident(Female, 1),
              ResidentialFloor(getResident(Female, 45), getResident(Female, 55), getAttic(1))
            )
          )
        )
      )

    assertResult(0)(Building.evenFloorsMenAvg(getBuilding("no_man", firstFloor)))
  }

}

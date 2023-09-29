package house

import house.house_exception._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class HouseSpec extends AnyFlatSpec with Matchers {
  val DEFAULT_TESTSET_SIZE = 100;
  val validHouseTypes: List[String] = List("economy", "premium")

  def generateInvalidHouseType: String = {
    val generated = Random.nextString(Random.nextInt(32))
    generated.toLowerCase match {
      case s if validHouseTypes.contains(s) => generateInvalidHouseType
      case _                                => generated
    }
  }

  "House constructor" should "create instance of House with given house type insensitive to upper- or lowercase letters" in {
    Seq("economy", "Economy", "ECONOMY", "eCONomY", "economY") foreach (line => {
      val house = House(line, Random.nextInt(1000), Random.nextInt(1000), Random.nextInt(1000), Random.nextInt(1000))
      assertResult("Economy")(house.getHouseType)
    })

    Seq("premium", "Premium", "PREMIUM", "pReMiUm", "PREmiuM") foreach (line => {
      val house = House(line, Random.nextInt(1000), Random.nextInt(1000), Random.nextInt(1000), Random.nextInt(1000))
      assertResult("Premium")(house.getHouseType)
    })
  }

  "House constructor" should "accept only declared house types and throw NoSuchHouseTypeException otherwise" in {
    for (_ <- 0 until DEFAULT_TESTSET_SIZE) {
      assertThrows[NoSuchHouseTypeException] {
        House(
          generateInvalidHouseType,
          Random.nextInt(1000),
          Random.nextInt(1000),
          Random.nextInt(1000),
          Random.nextInt(1000)
        )
      }
    }
  }

  "House constructor" should "create house with given floor amount, length, width, height" in {

    for (_ <- 0 until DEFAULT_TESTSET_SIZE) {
      val quad = getQuad
      val house = House(validHouseTypes(Random.nextInt(2)), quad._1, quad._2, quad._3, quad._4)
      assertResult(quad._1)(house.floors)
      assertResult(quad._2)(house.length)
      assertResult(quad._3)(house.width)
      assertResult(quad._4)(house.height)
    }
  }

  def getQuad: (Int, Int, Int, Int) = {
    (
      Random.nextInt(Int.MaxValue - 1) + 1,
      Random.nextInt(Int.MaxValue - 1) + 1,
      Random.nextInt(Int.MaxValue - 1) + 1,
      Random.nextInt(Int.MaxValue - 1) + 1
    )
  }

  "House constructor" should "throw instance of DimensionShouldBePositiveException if floors, length, width or height <=0" in {

    for (_ <- 0 until DEFAULT_TESTSET_SIZE) {
      val quad = getQuad
      assertThrows[DimensionShouldBePositiveException] {
        House(validHouseTypes(Random.nextInt(2)), -quad._1, quad._2, quad._3, quad._4)
      }
      assertThrows[DimensionShouldBePositiveException] {
        House(validHouseTypes(Random.nextInt(2)), quad._1, -quad._2, quad._3, quad._4)
      }
      assertThrows[DimensionShouldBePositiveException] {
        House(validHouseTypes(Random.nextInt(2)), quad._1, quad._2, -quad._3, quad._4)
      }
      assertThrows[DimensionShouldBePositiveException] {
        House(validHouseTypes(Random.nextInt(2)), quad._1, quad._2, quad._3, -quad._4)
      }
    }
  }

  "reconParquetCost" should "compute parquet cost for house depending on house type and floors amount" in {
    def economyCost: House => Long = (house: House) => house.length * house.width * house.height + 10000 * house.floors
    def premiumCost: House => Long =
      (house: House) => ((if (house.floors >= 5) 2 else 3) ^ house.floors) * (house.length + house.width + house.height)

    def costFunction = (house: House) =>
      house.getHouseType match {
        case "Economy" => economyCost(house)
        case _         => premiumCost(house)
      }

    for (_ <- 0 until DEFAULT_TESTSET_SIZE) {
      val quad = getQuad
      val house = House(validHouseTypes(Random.nextInt(2)), math.max(1, quad._1 % 32), quad._2, quad._3, quad._4)
      assertResult(costFunction(house))(house.reconParquetCost())
    }
  }

}

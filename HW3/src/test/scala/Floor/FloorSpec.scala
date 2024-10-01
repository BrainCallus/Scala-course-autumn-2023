package Floor

import resident._
import exceptions.BuildingError.IllegalCommercialCount
import floor.{Attic, Commercial, ResidentialFloor}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.PositiveInt

class FloorSpec extends AnyFlatSpec with Matchers {
  def getAttic(shops: Int): Attic = Attic(shops).getOrElse(null)

  def getResident(gender: Gender, age: Int): Resident = Resident(gender, age).getOrElse(null)

  "Attic constructor" should "create floor with 1 or none commercials" in {
    val attic: Attic = Attic(0).getOrElse(null)
    assert(attic.shops.isEmpty)
    val attic1 = Attic(1).getOrElse(null)
    assert(attic1.shops.get == PositiveInt(1))
  }

  it should "return IllegalCommercialCount if given value of shops is negative or >1" in {
    val attic = Attic(-3)
    assert(attic match {
      case Left(IllegalCommercialCount(_)) => true
      case _                               => false
    })

    val attic1 = Attic(2)
    assert(attic1 match {
      case Left(IllegalCommercialCount(_)) => true
      case _                               => false
    })
  }

  "Commercial constructor" should "create floor with given amount of commercials and upper floor" in {
    val commercial: Commercial = Commercial(55, getAttic(1)).getOrElse(null)
    assert(commercial.shops == PositiveInt(55))
    assert(commercial.up match {
      case _: Attic => true
      case _        => false
    })
  }

  it should "return IllegalCommercialCount if given value of shops is not positive" in {
    val commercial = Commercial(0, getAttic(1))
    assert(commercial match {
      case Left(IllegalCommercialCount(_)) => true
      case _                               => false
    })
  }

  "Residential constructor" should "create floor couple of residents and upper floor" in {
    val residential = ResidentialFloor(getResident(Male, 24), getResident(Female, 67), getAttic(0))
    assert(residential.residents._1.isMan)
    assert(residential.residents._1.age == PositiveInt(24))
    assert(!residential.residents._2.isMan)
    assert(residential.residents._2.age == PositiveInt(67))
    assert(residential.up match {
      case _: Attic => true
      case _        => false
    })
  }
}

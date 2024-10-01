package resident

import building._
import _root_.floor.{Attic, Commercial, Floor}
import exceptions.BuildingError.AgeError
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.PositiveInt

class ResidentSpec extends AnyFlatSpec with Matchers {
  def getBuilding(address: String, floor: Floor): Building = Building(address, floor).getOrElse(null)

  def getAttic(shops: Int): Attic = Attic(shops).getOrElse(null)

  def getCommercial(shops: Int, floor: Floor): Commercial = Commercial(shops, floor).getOrElse(null)

  def getResident(gender: Gender, age: Int): Resident = Resident(gender, age).getOrElse(null)

  "Resident constructor" should "create floor with given gender and age" in {
    val man: Resident = Resident(Male, 30).getOrElse(null)
    assert(man.isMan)
    assert(man.gender == Male)
    assert(man.age == PositiveInt(30))
    val woman: Resident = Resident(Female, 63).getOrElse(null)
    assert(!woman.isMan)
    assert(woman.gender == Female)
    assert(woman.age == PositiveInt(63))
  }

  it should "return AgeError if given value of age not positive" in {
    val resident = Resident(Male, 0)
    assert(resident match {
      case Left(AgeError) => true
      case _              => false
    })
  }

  "isMan" should "check whether resident is man, otherwise it's woman" in {
    val residentMan = Resident(Male, 77).getOrElse(null)
    assert(residentMan.isMan && residentMan.gender == Male)
    val residentWoman = Resident(Female, 4).getOrElse(null)
    assert(!residentWoman.isMan && residentWoman.gender == Female)
  }
}

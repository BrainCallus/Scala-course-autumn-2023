package typeparams

import scala.reflect.runtime.universe.TypeTag
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.language.{existentials, postfixOps}

class ServiceLevelAdvanceSpec extends AnyFlatSpec with Matchers {

  "advance" should "be able to leave the previous status" in {
    val ec = new ServiceLevelAdvance[Economy]
    assert(ec.advance[Economy].currentStatus match {
      case _: TypeTag[Economy] => true
      case _                   => false
    })

    val extend = new ServiceLevelAdvance[ExtendedEconomy]
    assert(extend.advance[ExtendedEconomy].currentStatus match {
      case _: TypeTag[ExtendedEconomy] => true
      case _                           => false
    })

    val business = new ServiceLevelAdvance[Business]
    assert(business.advance[Business].currentStatus match {
      case _: TypeTag[Business] => true
      case _                    => false
    })

    val platinum = new ServiceLevelAdvance[Platinum]
    assert(platinum.advance[Platinum].currentStatus match {
      case _: TypeTag[Platinum] => true
      case _                    => false
    })

    val elite = new ServiceLevelAdvance[Elite]
    assert(elite.advance[Elite].currentStatus match {
      case _: TypeTag[Elite] => true
      case _                 => false
    })

    val upgraded = new ServiceLevelAdvance[UpgradedEconomy]
    assert(upgraded.advance[UpgradedEconomy].currentStatus match {
      case _: TypeTag[UpgradedEconomy] => true
      case _                           => false
    })

    val special1b = new ServiceLevelAdvance[Special1b]
    assert(special1b.advance[Special1b].currentStatus match {
      case _: TypeTag[Special1b] => true
      case _                     => false
    })

  }

  "advance" should "can upgrade loyalty level from Economy to any other" in {
    val serv = new ServiceLevelAdvance[Economy]
    val extended = serv.advance[ExtendedEconomy]
    assert(extended.currentStatus match {
      case _: TypeTag[ExtendedEconomy] => true
      case _                           => false
    })
    val business = serv.advance[Business]
    assert(business.currentStatus match {
      case _: TypeTag[Business] => true
      case _                    => false
    })
    val platinum = serv.advance[Platinum]
    assert(platinum.currentStatus match {
      case _: TypeTag[Platinum] => true
      case _                    => false
    })
    val elite = serv.advance[Elite]
    assert(elite.currentStatus match {
      case _: TypeTag[Elite] => true
      case _                 => false
    })
    val upgraded = serv.advance[UpgradedEconomy]
    assert(upgraded.currentStatus match {
      case _: TypeTag[UpgradedEconomy] => true
      case _                           => false
    })
    val special = serv.advance[Special1b]
    assert(special.currentStatus match {
      case _: TypeTag[Special1b] => true
      case _                     => false
    })

  }

  "advance" should "be able upgrade loyalty level from UpgradedEconomy  to Special1b" in {
    val upgr = new ServiceLevelAdvance[UpgradedEconomy]
    val special1b = upgr.advance[Special1b]
    assert(special1b.currentStatus match {
      case _: TypeTag[Special1b] => true
      case _                     => false
    })
  }

  "advance" should "be able upgrade loyalty level from ExtendedEconomy to Business, Platinum and Elite" in {
    val extend = new ServiceLevelAdvance[ExtendedEconomy]
    val business = extend.advance[Business]
    assert(business.currentStatus match {
      case _: TypeTag[Business] => true
      case _                    => false
    })
    val platinum = extend.advance[Platinum]
    assert(platinum.currentStatus match {
      case _: TypeTag[Platinum] => true
      case _                    => false
    })
    val elite = extend.advance[Elite]
    assert(elite.currentStatus match {
      case _: TypeTag[Elite] => true
      case _                 => false
    })

  }

  "advance" should "be able upgrade loyalty level from Business to Platinum and Elite" in {
    val business = new ServiceLevelAdvance[Business]
    val platinum = business.advance[Platinum]
    assert(platinum.currentStatus match {
      case _: TypeTag[Platinum] => true
      case _                    => false
    })
    val elite = business.advance[Elite]
    assert(elite.currentStatus match {
      case _: TypeTag[Elite] => true
      case _                 => false
    })

  }

  "advance" should "be able to gradually increase loyalty level from Economy to Platinum" in {
    val economy = new ServiceLevelAdvance[Economy]
    assert(economy.currentStatus match {
      case _: TypeTag[Economy] => true
      case _                   => false
    })
    val extended = economy.advance[ExtendedEconomy]
    assert(extended.currentStatus match {
      case _: TypeTag[ExtendedEconomy] => true
      case _                           => false
    })
    val business = extended.advance[Business]
    assert(business.currentStatus match {
      case _: TypeTag[Business] => true
      case _                    => false
    })
    val platinum = business.advance[Platinum]
    assert(platinum.currentStatus match {
      case _: TypeTag[Platinum] => true
      case _                    => false
    })
  }

  "advance" should "be able to gradually increase loyalty level from Economy to Elite" in {
    val economy = new ServiceLevelAdvance[Economy]
    assert(economy.currentStatus match {
      case _: TypeTag[Economy] => true
      case _                   => false
    })
    val extended = economy.advance[ExtendedEconomy]
    assert(extended.currentStatus match {
      case _: TypeTag[ExtendedEconomy] => true
      case _                           => false
    })
    val business = extended.advance[Business]
    assert(business.currentStatus match {
      case _: TypeTag[Business] => true
      case _                    => false
    })
    val elite = business.advance[Elite]
    assert(elite.currentStatus match {
      case _: TypeTag[Elite] => true
      case _                 => false
    })
  }

  "advance" should "be able to gradually increase loyalty level from Economy to Special1b" in {
    val economy = new ServiceLevelAdvance[Economy]
    assert(economy.currentStatus match {
      case _: TypeTag[Economy] => true
      case _                   => false
    })
    val upgraded = economy.advance[UpgradedEconomy]
    assert(upgraded.currentStatus match {
      case _: TypeTag[UpgradedEconomy] => true
      case _                           => false
    })
    val special1b = upgraded.advance[Special1b]
    assert(special1b.currentStatus match {
      case _: TypeTag[Special1b] => true
      case _                     => false
    })
  }

  "advance" should "not be able to downgrade loyalty level from ExtendedEconomy to Economy" in {
    assertDoesNotCompile("new ServiceLevelAdvance[ExtendedEconomy].advance[Economy]")
  }

  "advance" should "not be able to downgrade loyalty level from Business to ExtendedEconomy, Economy" in {
    assertDoesNotCompile("new ServiceLevelAdvance[Business].advance[ExtendedEconomy]")
    assertDoesNotCompile("new ServiceLevelAdvance[Business].advance[Economy]")
  }

  "advance" should "not be able to downgrade loyalty level from Platinum to Business, ExtendedEconomy, Economy" in {
    assertDoesNotCompile("new ServiceLevelAdvance[Platinum].advance[Business]")
    assertDoesNotCompile("new ServiceLevelAdvance[Platinum].advance[ExtendedEconomy]")
    assertDoesNotCompile("new ServiceLevelAdvance[Platinum].advance[Economy]")
  }

  "advance" should "not be able to downgrade loyalty level from Elite to Business, ExtendedEconomy, Economy" in {
    assertDoesNotCompile("new ServiceLevelAdvance[Elite].advance[Business]")
    assertDoesNotCompile("new ServiceLevelAdvance[Elite].advance[ExtendedEconomy]")
    assertDoesNotCompile("new ServiceLevelAdvance[Elite].advance[Economy]")
  }

  "advance" should "not be able to downgrade loyalty level from UpgradedEconomy to Economy" in {
    assertDoesNotCompile("new ServiceLevelAdvance[UpgradedEconomy].advance[Economy]")
  }

  "advance" should "not be able to downgrade loyalty level from Special1b  to UpgradedEconomy, Economy" in {
    assertDoesNotCompile("new ServiceLevelAdvance[Special1b].advance[UpgradedEconomy]")
    assertDoesNotCompile("new ServiceLevelAdvance[Special1b].advance[Economy]")
  }

  "advance" should "not be able to migrate from ExtendedEconomy to UpgradedEconomy, Special1b" in {
    assertDoesNotCompile("new ServiceLevelAdvance[ExtendedEconomy].advance[UpgradedEconomy]")
    assertDoesNotCompile("new ServiceLevelAdvance[ExtendedEconomy].advance[Special1b]")
  }

  "advance" should "not be able to migrate from Business to UpgradedEconomy, Special1b" in {
    assertDoesNotCompile("new ServiceLevelAdvance[Business].advance[UpgradedEconomy]")
    assertDoesNotCompile("new ServiceLevelAdvance[Business].advance[Special1b]")
  }

  "advance" should "not be able to migrate from Platinum to UpgradedEconomy, Special1b" in {
    assertDoesNotCompile("new ServiceLevelAdvance[Platinum].advance[UpgradedEconomy]")
    assertDoesNotCompile("new ServiceLevelAdvance[Platinum].advance[Special1b]")
  }

  "advance" should "not be able to migrate from Elite to UpgradedEconomy, Special1b" in {
    assertDoesNotCompile("new ServiceLevelAdvance[Elite].advance[UpgradedEconomy]")
    assertDoesNotCompile("new ServiceLevelAdvance[Elite].advance[Special1b]")
  }

  "advance" should "not be able to migrate from Elite to Platinum and vise versa" in {
    assertDoesNotCompile("new ServiceLevelAdvance[Elite].advance[Platinum]")
    assertDoesNotCompile("new ServiceLevelAdvance[Platinum].advance[Elite]")
  }

  "advance" should "not be able to migrate from UpgradedEconomy to ExtendedEconomy, Business, Platinum, Elite" in {
    assertDoesNotCompile("new ServiceLevelAdvance[UpgradedEconomy].advance[ExtendedEconomy]")
    assertDoesNotCompile("new ServiceLevelAdvance[UpgradedEconomy].advance[Business]")
    assertDoesNotCompile("new ServiceLevelAdvance[UpgradedEconomy].advance[Platinum]")
    assertDoesNotCompile("new ServiceLevelAdvance[UpgradedEconomy].advance[Elite]")
  }

  "advance" should "not be able to migrate from Special1b to ExtendedEconomy, Business, Platinum, Elite" in {
    assertDoesNotCompile("new ServiceLevelAdvance[Special1b].advance[ExtendedEconomy]")
    assertDoesNotCompile("new ServiceLevelAdvance[Special1b].advance[Business]")
    assertDoesNotCompile("new ServiceLevelAdvance[Special1b].advance[Platinum]")
    assertDoesNotCompile("new ServiceLevelAdvance[Special1b].advance[Elite]")
  }
}

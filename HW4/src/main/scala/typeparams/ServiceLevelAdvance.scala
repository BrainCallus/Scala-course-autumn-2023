package typeparams

import scala.reflect.runtime.universe.TypeTag

class Economy

class UpgradedEconomy extends Economy

class Special1b extends UpgradedEconomy

class ExtendedEconomy extends Economy

class Business extends ExtendedEconomy

class Elite extends Business

class Platinum extends Business

class ServiceLevelAdvance[T <: Economy](implicit val currentStatus: TypeTag[T]) {
  def advance[E <: T: TypeTag]: ServiceLevelAdvance[E] = {
    new ServiceLevelAdvance[E]
  }
}

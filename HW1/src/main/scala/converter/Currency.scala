package converter

import converter.Utils.{levenshteinDistance, stringJoin}
import converter.errors.UnsupportedCurrencyException

/** Enumeration represents currencies that supported. To provide type-level protection against illegal currency values
  * for operations with [[converter.Money]]
  */
object Currency extends Enumeration {
  type Currency = Value
  val RUB, USD, EUR = Value

  /** Let someone shoot off his knee
    *
    * @param name
    *   expected currency string value
    * @return
    *   Option[Currency.Value]
    */
  def getByName(name: String): Option[Value] =
    try {
      Some(withName(name))
    } catch {
      case _: Exception => None
    }

  /** same to [[scala.Enumeration#withName(java.lang.String)]] but on fail throws
    * [[converter.errors.UnsupportedCurrencyException]]
    *
    * @param name
    *   string value of some converter.Currency#Currency
    * @return
    *   converter.Currency#Currency with given name
    * @throws UnsupportedCurrencyException
    *   if given `name` doesn't match any [[converter.Currency#Currency]]
    */
  def getByNameOrThrow(name: String): Value = {
    getByName(name) match {
      case Some(value) => value
      case None        => throw new UnsupportedCurrencyException(name, suggestAnotherOrAvailable(name))
    }
  }

  private def valuesAsIterableString(currencyValues: Iterable[Currency]): Iterable[String] = {
    currencyValues map (v => v.toString)
  }

  /** Transforms given values to String delimiting them with "\r\n"
    *
    * @param currencyValues
    *   collection([[Iterable]]) of [[converter.Currency#Currency]]
    * @return
    *   `currencyValues` as string delimited with "\r\n"
    */
  def getAvailableAsListString(currencyValues: Iterable[Currency]): String =
    stringJoin(valuesAsIterableString(currencyValues), "\r\n")

  /** For given value find most closest [[converter.Currency#Currency]] or show all supported currencies
    *
    * Try to define [[converter.Currency#Currency]] that string value differ by levenshtein distance from any of
    * [[converter.Currency#Currency]] not more than it's length and, in case success return that value, otherwise show
    * list of all available
    *
    * @param unsupportedValue
    *   value to which makes search
    * @return
    *   most closest by levenshtein distance [[converter.Currency#Currency]] or list of all supported values
    */
  def suggestAnotherOrAvailable(unsupportedValue: String): String = {
    valuesAsIterableString(values) map (v => (levenshteinDistance(unsupportedValue, v), v)) filter
      (entry => entry._1 <= entry._2.length / 2) minByOption (_._1) match {
      case Some(value) => s"Maybe you mean \"${value._2}\"?"
      case None        => "Currencies available:\r\n" + getAvailableAsListString(values)
    }
  }
}

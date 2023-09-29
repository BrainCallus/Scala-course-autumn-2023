package converter

import converter.Currency.{Currency, getAvailableAsListString, getByNameOrThrow}
import converter.Money.testPositive
import converter.Utils.testOrThrow
import converter.errors._

class CurrencyConverter(ratesDictionary: Map[Currency, Map[Currency, BigDecimal]]) {

  /** converts given money to another given currency
    *
    * `toCurrency` shouldn't be equal to currency of given money. Note that
    * [[converter.CurrencyConverter#ratesDictionary]] could be antisymmetric and could not contain options for concrete
    * ordered currency pair
    *
    * @param money
    *   money to exchange
    * @param toCurrencyStr
    *   currency `money` exchanged to
    * @return
    *   money that currency is `toCurrencyStr`
    * @throws UnsupportedCurrencyException
    *   if `toCurrencyStr` doesn't match any [[converter.Currency#Currency]] or if
    *   [[converter.CurrencyConverter#ratesDictionary]] not contain options for concrete ordered currency pair
    * @throws WrongCurrencyException
    *   if money.currency and toCurrencyStr are equal
    */
  def exchange(money: Money, toCurrencyStr: String): Money = {
    val toCurrency = testOrThrow(
      money.currency.equals,
      getByNameOrThrow(toCurrencyStr),
      new WrongCurrencyException("It is an absurd to convert some currency to itself!")
    )
    Money(money.amount * getExchangeRate(money.currency, toCurrency), toCurrency.toString)
  }

  private def getExchangeRate(from: Currency, to: Currency): BigDecimal =
    try {
      val fromMap = ratesDictionary.apply(from)
      try {
        fromMap.apply(to)
      } catch {
        case _: NoSuchElementException =>
          throw new UnsupportedCurrencyException(
            to.toString,
            s"$from convertible only to:\n" + getAvailableAsListString(fromMap.keySet)
          )
      }
    } catch {
      case _: NoSuchElementException =>
        throw new UnsupportedCurrencyException(
          from.toString,
          s"For convert from available:\n" + getAvailableAsListString(ratesDictionary.keySet)
        )
    }
}

object CurrencyConverter {

  /** @constructor
    *   for [[converter.CurrencyConverter]]
    *
    * Tests that all keys match any [[converter.Currency#Currency]] and all values not less than 0
    * @param dictionary
    *   [[Map[String, Map[String, BigDecimal]] describes exchange rates
    * @return
    *   instance of [[converter.CurrencyConverter]]
    * @throws UnsupportedCurrencyException
    *   if some of string keys don't match any [[converter.Currency#Currency]]
    * @throws MoneyAmountShouldBePositiveException
    *   if some of values in specified map are negative
    */
  def apply(dictionary: Map[String, Map[String, BigDecimal]]): CurrencyConverter = {
    // sorry for this code-fir but this simplifies the brackets sequences control
    new CurrencyConverter(
      changeKeyTypeToCurrency(
        dictionary map
          (entry =>
            (
              entry._1,
              changeKeyTypeToCurrency(
                entry._2 map (inner => (inner._1, testPositive(inner._2)))
              )
            )
          )
      )
    )
  }

  private def changeKeyTypeToCurrency[T](initial: Map[String, T]): Map[Currency, T] =
    Utils.mapStringKeyMapByKey(initial, getByNameOrThrow)
}

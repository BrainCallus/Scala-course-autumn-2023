package converter

import converter.Currency.{Currency, getByNameOrThrow}
import converter.Money.testPositive
import converter.Utils.{not, testOrThrow}
import converter.errors.{MoneyAmountShouldBePositiveException, WrongCurrencyException, UnsupportedCurrencyException}

import java.util.function.BinaryOperator

/** Represents money with amount and currency
  *
  * @param amount
  *   amount of this money
  * @param currency
  *   instance of [[converter.Currency#Currency]]
  */
final case class Money private (amount: BigDecimal, currency: Currency) {

  /** Increases amount
    *
    * @param other
    *   money to which amount current amount increased
    * @return
    *   money with increased amount
    * @throws WrongCurrencyException
    *   if currency of `other` differ to this
    */
  def +(other: Money): Money = Money(changeAmount(_ + _, other.amount), getCurrencyForAnotherMoney(other))

  /** Decreases amount
    *
    * @param other
    *   money to which amount current amount decreased
    * @return
    *   money with decreased amount
    * @throws WrongCurrencyException
    *   if currency of `other` differ to this
    * @throws MoneyAmountShouldBePositiveException
    *   if result of subtract is negative
    */
  def -(other: Money): Money = Money(changeAmount(_ - _, other.amount), getCurrencyForAnotherMoney(other))

  /** Checks whether given money has the same currency whit this
    *
    * @param other
    *   money which currency checks
    * @return
    *   true if currencies are the same
    */
  def isSameCurrency(other: Money): Boolean = {
    other.currency == currency
  }

  private def changeAmount(oper: BinaryOperator[BigDecimal], delta: BigDecimal): BigDecimal =
    testPositive(oper.apply(amount, delta))

  private def getCurrencyForAnotherMoney(other: Money): Currency =
    testOrThrow(
      not(isSameCurrency),
      other,
      new WrongCurrencyException(s"Unable to change amount. Expected $currency, found ${other.currency}")
    ).currency
}

object Money {

  /** public @constructor for [[converter.Money]]
    *
    * Checks that given string value `currency` matches any [[converter.Currency#Currency]] and `amount` not less than 0
    *
    * @param amount
    *   anount of money
    * @param currency
    *   string representation of some [[converter.Currency#Currency]]
    * @return
    *   constructed instance of [[converter.Money]]
    * @throws UnsupportedCurrencyException
    *   if entered String `currency` doesn't matches any [[converter.Currency#Currency]];
    * @throws MoneyAmountShouldBePositiveException
    *   if given `amount` less than 0
    */
  def apply(amount: BigDecimal, currency: String): Money =
    new Money(testPositive(amount), getByNameOrThrow(currency))

  /** Tests that given value is not less than zero
    *
    * @param amount
    *   value to check
    * @return
    *   given value if it is positive
    * @throws MoneyAmountShouldBePositiveException
    *   if given value is negative
    */
  def testPositive(amount: BigDecimal): BigDecimal =
    testOrThrow((x: BigDecimal) => x < 0, amount, new MoneyAmountShouldBePositiveException)
}

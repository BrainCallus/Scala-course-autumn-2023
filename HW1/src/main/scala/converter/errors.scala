package converter

/** Object contains exceptions that possibly thrown in operations with [[converter.Money]],
  * [[converter.CurrencyConverter]], [[converter.Currency]]
  */
object errors {
  class MoneyAmountShouldBePositiveException(private val message: String = "Money amount should be positive")
    extends Exception(message)

  class UnsupportedCurrencyException(private val message: String) extends Exception(message) {
    def this() = this("Currency you entered is unsupported")

    def this(unsupportedValue: String, referenceInfo: String) =
      this(s"Currency $unsupportedValue is unsupported\n$referenceInfo")
  }

  class WrongCurrencyException(private val message: String) extends Exception(message)
}

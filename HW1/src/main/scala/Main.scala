import converter.errors.{UnsupportedCurrencyException, WrongCurrencyException}
import converter._

/** examples
  */
object Main {

  def main(args: Array[String]): Unit = {
    val converter = CurrencyConverter(
      Map(
        "USD" -> Map("RUB" -> BigDecimal(94.5), "EUR" -> BigDecimal(1 / 1.1)),
        "RUB" -> Map("USD" -> BigDecimal(1 / 99.8), "EUR" -> BigDecimal(1 / 108.55)),
        "EUR" -> Map("RUB" -> BigDecimal(102.43), "USD" -> BigDecimal(1.02))
      )
    )
    var money = converter.exchange(Money(10000, "RUB"), "USD")
    println(
      "Percentage loss of straight RUB <-> USD exchange for this converter is " +
        (100 - converter.exchange(money, "RUB").amount / 100).setScale(2, BigDecimal.RoundingMode.HALF_UP) + "%"
    )
    val usdBalance = money.amount
    money = converter.exchange(money, "EUR")
    println(
      "Percentage loss of straight USD <-> EUR exchange for this converter is " +
        (100 - converter.exchange(money, "USD").amount / usdBalance * 100).setScale(2, BigDecimal.RoundingMode.HALF_UP)
        + "%"
    )
    val eurBalance = money.amount
    money = converter.exchange(money, "RUB")
    println(
      "Percentage loss of straight EUR <-> RUB exchange for this converter is " +
        (100 - converter.exchange(money, "EUR").amount / eurBalance * 100).setScale(2, BigDecimal.RoundingMode.HALF_UP)
        + "%"
    )
    println(
      "Total loss of cyclic exchange RUB -> USD -> EUR -> RUB is " +
        (100 - money.amount / 100).setScale(2, BigDecimal.RoundingMode.HALF_UP) + "%"
    )
    try {
      converter.exchange(money, "RUB")
    } catch {
      case e: WrongCurrencyException => println(e.getMessage)
    }
    try {
      converter.exchange(money, "eur")
    } catch {
      // expected list available
      case e: UnsupportedCurrencyException => println(e.getMessage)
    }

    try {
      converter.exchange(money, "US")
    } catch {
      // expected suggested "USD"
      case e: UnsupportedCurrencyException => println(e.getMessage)
    }
  }
}

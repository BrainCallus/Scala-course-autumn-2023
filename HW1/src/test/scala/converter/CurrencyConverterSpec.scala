package converter

import converter.Currency.{getAvailableAsListString, getByName, getByNameOrThrow, suggestAnotherOrAvailable}
import converter.errors.{MoneyAmountShouldBePositiveException, UnsupportedCurrencyException, WrongCurrencyException}
import converter.Utils._
import org.scalatest._
import flatspec._
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.collection.mutable.ListBuffer
import scala.util.Random

class CurrencyConverterSpec extends AnyFlatSpec with Matchers {

  val DEFAULT_TEST_SIZE = 100

  val modelRates: Map[String, Map[String, BigDecimal]] = Map(
    "USD" -> Map("RUB" -> BigDecimal(94.5), "EUR" -> BigDecimal(1 / 1.1)),
    "RUB" -> Map("USD" -> BigDecimal(1 / 99.8), "EUR" -> BigDecimal(1 / 108.55)),
    "EUR" -> Map("RUB" -> BigDecimal(102.43), "USD" -> BigDecimal(1.02))
  )

  val validCurrencies: List[String] = List("EUR", "RUB", "USD")

  def generateRandomString(minLength: Integer, maxLength: Integer): String =
    Random.nextString(Random.nextInt(maxLength - minLength + 1) + minLength)

  def generateInvalidCurrencyValue: String = {
    var s = generateRandomString(0, 64)
    while (validCurrencies.contains(s)) {
      s = generateRandomString(0, 64)
    }
    s
  }

  "levenshteinDistance" should "compute levenshtein distance for strings" in {
    levenshteinDistance("aaaa", "bbbb") shouldEqual 4
    levenshteinDistance("abc", "aabb") shouldEqual 2
    levenshteinDistance("constitution", "prostitution") shouldEqual 3
    levenshteinDistance("abcdef", "fedcba") shouldEqual levenshteinDistance("fedcba", "abcdef")
    levenshteinDistance("RUB", "RU") < levenshteinDistance("RU", "EUR") shouldBe true
    levenshteinDistance("eur", "EUR") shouldEqual 3
    levenshteinDistance("UD", "USD") shouldEqual levenshteinDistance("USD", "USDT")
    levenshteinDistance("USD", "US") < levenshteinDistance("USDTR", "USD")
    levenshteinDistance("aaaardvark", "aardvark") shouldEqual 2
    levenshteinDistance("AAAArdvArk", "aardvark") shouldEqual 5
    levenshteinDistance("avarice", "vase") shouldEqual 4
    levenshteinDistance("strange", "rangers") shouldEqual 4

  }

  "suggestAnotherOrAvailable" should "suggest closest currency value or show all available currencies if levenshtein distance to big" in {
    def isAnotherValueSuggested(badCurrency: String): Boolean =
      suggestAnotherOrAvailable(badCurrency) startsWith "Maybe you mean "

    def generateValueList(predicate: (String => Boolean) => Boolean): ListBuffer[String] = {
      val invalidValues = new ListBuffer[String]()
      while (invalidValues.length < DEFAULT_TEST_SIZE) {
        val s = (Iterator continually Random.nextPrintableChar() filter (ch => ch.isLetter || ch.isDigit) take
          Random.nextInt(20)).mkString
        if (predicate.apply((currency: String) => levenshteinDistance(currency, s) < 2)) {
          invalidValues += s
        }
      }
      invalidValues
    }

    for (s: String <- generateValueList(validCurrencies.exists)) {
      isAnotherValueSuggested(s) shouldBe true
    }

    generateInvalidCurrencyValue
    for (s <- generateValueList(Utils.not(validCurrencies.exists))) {
      isAnotherValueSuggested(s) shouldBe false
    }

  }

  "getByName" should "be noExcept method returns Some(Currency value) or None if input doesn't match any value" in {
    assertResult(Some(Currency.USD))(getByName("USD"))
    assertResult(Some(Currency.EUR))(getByName("EUR"))
    assertResult(Some(Currency.RUB))(getByName("RUB"))
    for (_ <- 0 until 100) {
      assertResult(None)(getByName(generateInvalidCurrencyValue))
    }
    assertResult(None)(getByName(null))
  }

  "getByNameOrThrow" should "throw exception if input doesn't match any value of Currency.values" in {
    Seq("rub", "RU", "usd", "US", "eur", "EU", "GBP", "JPY", "CAD", "", null, "BTC").foreach(currency =>
      assertThrows[UnsupportedCurrencyException] {
        getByNameOrThrow(currency)
      }
    )
    for (_ <- 0 until 100) {
      assertThrows[UnsupportedCurrencyException] {
        getByNameOrThrow(generateInvalidCurrencyValue)
      }
    }
  }

  "getAvailableAsListString" should "return string contains all string values of currencies in given collection delimited with \" \\r\\n\"" in {
    val sample: String = "USD\r\nRUB\r\nEUR"
    getAvailableAsListString(List(Currency.USD, Currency.RUB, Currency.EUR)) shouldEqual sample
    getAvailableAsListString(Seq(Currency.USD, Currency.RUB, Currency.EUR)) shouldEqual sample
    getAvailableAsListString(Set(Currency.USD, Currency.RUB, Currency.EUR)) shouldEqual sample
    getAvailableAsListString(Array(Currency.USD, Currency.RUB, Currency.EUR)) shouldEqual sample
  }

  "converted constructor" should "throw MoneyAmountShouldBePositiveException if rates dictionary contains negative values" in {
    val invalidRates = modelRates map (entry => (entry._1, entry._2 map (e => (e._1, -e._2))))
    assertThrows[MoneyAmountShouldBePositiveException] {
      CurrencyConverter(invalidRates)
    }
  }

  "converted constructor" should "throw UnsupportedCurrencyException if rates dictionary contains wrong currency" in {
    var invalidRates = modelRates
    invalidRates += ("GBP" -> Map("RUB" -> BigDecimal(85)))
    assertThrows[UnsupportedCurrencyException] {
      CurrencyConverter(invalidRates)
    }
  }

  "exchange" should "convert money for supported currencies" in {
    val rates = modelRates
    val converter = CurrencyConverter(rates)
    val exchangedRub = converter.exchange(Money(2, "USD"), "RUB")
    val exchangedUsd = converter.exchange(Money(10, "RUB"), "USD")
    val exchangedEur = converter.exchange(Money(10, "USD"), "EUR")
    exchangedRub.amount shouldEqual 189
    exchangedRub.currency.toString shouldEqual "RUB"
    exchangedUsd.amount shouldEqual BigDecimal(1 / 9.98)
    exchangedUsd.currency.toString shouldEqual "USD"
    /* in map we lose calculation accuracy. Maybe better to use Tuple(BigDecimal, BigDecimal)
   instead of already calculated fraction
     */
    exchangedEur.amount shouldEqual BigDecimal(1 / 1.1) * 10
    exchangedEur.currency.toString shouldEqual "EUR"

    val initCyclic = Money(10000, "RUB")
    val exchangedCyclic = converter.exchange(converter.exchange(converter.exchange(initCyclic, "USD"), "EUR"), "RUB")
    assertResult(Currency.RUB)(exchangedCyclic.currency)
    assertResult(
      BigDecimal(10000 / 99.8 / 1.1 * 102.43).setScale(6, BigDecimal.RoundingMode.HALF_UP)
    )(exchangedCyclic.amount.setScale(6, BigDecimal.RoundingMode.HALF_UP))
  }

  "exchange" should "throw UnsupportedCurrencyException if rates dictionary doesn't contain currency" in {
    var converter = CurrencyConverter(
      modelRates filter (entry => !entry._1.equals("EUR")) map
        (entry => (entry._1, entry._2 filter (innerEntry => !innerEntry._1.equals("EUR"))))
    )
    assertThrows[UnsupportedCurrencyException] {
      converter.exchange(Money(10, "USD"), "EUR")
      converter.exchange(Money(2, "EUR"), "RUB")
    }

    converter = CurrencyConverter(modelRates filter (entry => !entry._1.equals("RUB")))
    assertResult(94.5)(converter.exchange(Money(1, "USD"), "RUB").amount)
    assertThrows[UnsupportedCurrencyException] {
      converter.exchange(Money(100, "RUB"), "USD")
    }

    converter = CurrencyConverter(
      modelRates map (entry => (entry._1, entry._2 filter (innerEntry => !innerEntry._1.equals("RUB"))))
    )
    assertResult(1 / 0.998)(converter.exchange(Money(100, "RUB"), "USD").amount)
    assertThrows[UnsupportedCurrencyException] {
      converter.exchange(Money(1, "USD"), "RUB")
    }
  }

  "exchange" should "throws WrongCurrencyException if initial and target currencies are equal" in {
    val converter = CurrencyConverter(modelRates)
    for (i <- 0 until 3) {
      assertThrows[WrongCurrencyException] {
        converter.exchange(Money(Random.nextInt(100), validCurrencies(i)), validCurrencies(i))
      }
    }
  }

  def assertMoneyException(exceptionType: Exception, parameters: Map[BigDecimal, String]): Unit = {
    parameters.foreach(entry =>
      assertThrows[exceptionType.type] {
        Money(entry._1, entry._2)
      }
    )
  }

  "Money constructor" should "throw UnsupportedCurrencyException when entered currency doesn't match any Currency.Value" in {
    val e = new UnsupportedCurrencyException
    assertMoneyException(
      e,
      Array
        .fill(100) {
          (BigDecimal(Random.between(0, Double.PositiveInfinity)), generateInvalidCurrencyValue)
        }
        .toMap
    )
  }

  "Money constructor" should "throw MoneyAmountShouldBePositiveException when entered amount < 0" in {
    Money(-0.00000, "RUB") // shouldn't fail
    val e = new MoneyAmountShouldBePositiveException
    assertMoneyException(
      e,
      Array
        .fill(100) {
          (BigDecimal(Random.between(Double.NegativeInfinity, 0)), validCurrencies(Random.nextInt(3)))
        }
        .toMap
    )
  }

  "Money `+`" should "increase amount" in {
    var money = Money(10, "RUB")
    var delta = 0
    for (i <- 0 to 100) {
      delta += i
      money = money.+(Money(i, "RUB"))
    }
    assertResult(5050)(delta)
    assertResult(10 + delta)(money.amount)
  }

  "Money `-`" should "decrease amount" in {
    var money = Money(6000, "RUB")
    var delta = 0
    for (i <- 0 to 100) {
      delta += i
      money = money.-(Money(i, "RUB"))
    }
    assertResult(5050)(delta)
    assertResult(6000 - delta)(money.amount)
  }

  "Money `+` and `-`" should "throw WrongCurrencyException if currencies are different" in {
    val money = Money(10000, "USD")
    for (_ <- 0 until 50) {
      assertThrows[WrongCurrencyException] {
        money.+(
          Money(
            Random.between(0, Double.PositiveInfinity),
            validCurrencies(Random.nextInt(2))
          )
        )
      }
      assertThrows[WrongCurrencyException] {
        money.-(Money(Random.between(0, 10000), validCurrencies(Random.nextInt(2))))
      }
    }
  }

  "Money `-`" should "throw MoneyAmountShouldBePositiveException when subtracted amount less then amount on balance" in {
    val money = Money(5000 - (-Random.between(-1, 0)), "USD")
    for (_ <- 0 until 100) {
      assertThrows[MoneyAmountShouldBePositiveException] {
        money.-(Money(Random.between(5000, Integer.MAX_VALUE - 1), "USD"))
      }
    }
  }

  "isSameCurrency" should "check whether currencies are the same" in {
    validCurrencies.foreach(currency => {
      assertResult(true)(
        Money(Random.between(0, 100), currency) isSameCurrency
          Money(Random.between(0, 100), currency)
      )
    })
    for (i <- 0 until 3) {
      val money = Money(Random.between(0, 100), validCurrencies(i))
      assertResult(false)(
        money isSameCurrency
          Money(Random.between(0, 100), validCurrencies((i + 1) % validCurrencies.length))
      )
      assertResult(false)(
        money isSameCurrency
          Money(Random.between(0, 100), validCurrencies((i + 2) % validCurrencies.length))
      )
    }
  }

}

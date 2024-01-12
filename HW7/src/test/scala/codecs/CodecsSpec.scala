package codecs

import cats.syntax.show._
import codecs.Json._
import codecs.JsonReader._
import codecs.JsonWriter._
import codecs.Person._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.HashMap

class CodecsSpec extends AnyFlatSpec with Matchers {
  "string" should "be parsed as json value" in {
    "String".toJson shouldEqual JsonString("String")
  }

  "string" should "be read from JsonString" in {
    JsonString("String").as[String] shouldEqual (Right("String"))
  }

  "integer" should "be parsed as json value" in {
    1234.toJson shouldEqual JsonInt(1234)
  }

  "integer" should "be read from JsonInt" in {
    JsonInt(1234).as[Int] shouldEqual Right(1234)
  }

  "double" should "be parsed as json value" in {
    2.5.toJson shouldEqual JsonDouble(2.5)
  }

  "double" should "be read from JsonDouble" in {
    JsonDouble(2.5).as[Double] shouldEqual Right(2.5)
  }

  "None" should "be parsed as JsonNull" in {
    None.toJson shouldEqual JsonNull
  }

  "null" should "be read as None" in {
    JsonNull.as[Option[Int]] shouldEqual Right(None)
  }

  "list of strings" should "be parsed as json value" in {
    List("Kek", "Shrek").toJson shouldEqual JsonArray(List(JsonString("Kek"), JsonString("Shrek")))
  }

  "list of strings" should "be read from JsonArray" in {
    JsonArray(List(JsonString("Kek"), JsonString("Shrek"))).as[List[String]] shouldEqual
      Right(List("Kek", "Shrek"))
  }

  "list of universities" should "be read from JsonArray" in {
    val innoJson = JsonObject(
      Map(
        "name" -> JsonString("Inno"),
        "city" -> JsonString("Inno"),
        "country" -> JsonString("Russia"),
        "qsRank" -> JsonInt(214)
      )
    )
    val mitJson = JsonObject(
      Map(
        "name" -> JsonString("MIT"),
        "city" -> JsonString("Massachusetts"),
        "country" -> JsonString("USA"),
        "qsRank" -> JsonInt(1)
      )
    )
    JsonArray(List(innoJson, mitJson)).as[List[University]] shouldEqual
      Right(
        List(
          University("Inno", "Inno", "Russia", 214),
          University("MIT", "Massachusetts", "USA", 1)
        )
      )
  }

  "student" should "be parsed as json object" in {
    Student("Max", 21, University("Inno", "Inno", "Russia", 214)).toJson shouldEqual JsonObject(
      Map(
        "name" -> JsonString("Max"),
        "age" -> JsonInt(21),
        "university" -> JsonObject(
          Map(
            "name" -> JsonString("Inno"),
            "city" -> JsonString("Inno"),
            "country" -> JsonString("Russia"),
            "qsRank" -> JsonInt(214)
          )
        )
      )
    )
  }

  "student" should "be pretty printed using Show instance" in {
    val studentJson = Student("Max", 21, University("Inno", "Inno", "Russia", 214)).toJson
    studentJson.show.replaceAll("\\s+", "") shouldEqual
      """{"name": "Max",
         "age": 21,
         "university": {
         "name": "Inno",
         "city": "Inno",
         "country": "Russia",
         "qsRank": 214
         }
         }""".replaceAll("\\s+", "")
  }

  "manager" should "be parsed as json object" in {
    val employees = List(Employee("Andy", 23, 40000))
    Manager("Max", 30, 60000, employees, None).toJson shouldEqual JsonObject(
      HashMap(
        "name" -> JsonString("Max"),
        "age" -> JsonInt(30),
        "salary" -> JsonDouble(60000),
        "employees" -> JsonArray(
          List(
            JsonObject(
              Map(
                "name" -> JsonString("Andy"),
                "age" -> JsonInt(23),
                "salary" -> JsonDouble(40000)
              )
            )
          )
        ),
        "boss" -> JsonNull
      )
    )
  }

  "manager" should "be parsed from Json" in {
    JsonObject(
      Map(
        "name" -> JsonString("Max"),
        "age" -> JsonInt(30),
        "salary" -> JsonDouble(60000),
        "employees" -> JsonArray(
          List(
            JsonObject(
              Map(
                "name" -> JsonString("Andy"),
                "age" -> JsonInt(23),
                "salary" -> JsonDouble(40000)
              )
            )
          )
        )
      )
    ).as[Manager] shouldEqual Right(Manager("Max", 30, 60000, List(Employee("Andy", 23, 40000)), None))
  }
}

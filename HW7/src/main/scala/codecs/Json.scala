package codecs

import cats.Show

sealed trait Json
object Json {
  final case object JsonNull extends Json
  final case class JsonString(value: String) extends Json
  final case class JsonInt(value: Int) extends Json
  final case class JsonDouble(value: Double) extends Json
  final case class JsonArray(value: List[Json]) extends Json
  final case class JsonObject(value: Map[String, Json]) extends Json

  implicit val show: Show[Json] = {
    case JsonNull          => "null"
    case JsonString(str)   => s"\"$str\""
    case JsonInt(value)    => value.toString
    case JsonDouble(value) => value.toString
    case JsonArray(value)  => (value map show.show).mkString("[", ", ", "]")
    case Json.JsonObject(value) =>
      (value map (entry => s"\"${entry._1}\": ${show.show(entry._2)}")).mkString("{\n", ",\n", "\n}")
  } // or just (t: Json) => ??? (called Single Abstract Method)
}

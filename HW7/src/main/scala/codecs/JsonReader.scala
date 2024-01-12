package codecs

import codecs.Json.{JsonArray, JsonDouble, JsonInt, JsonNull, JsonObject, JsonString}

trait JsonReader[A] {
  def read(json: Json): Either[ReaderError, A]
}

object JsonReader {
  // Summoner function
  def apply[A](implicit reader: JsonReader[A]): JsonReader[A] = reader

  implicit class JsonReaderOps(val json: Json) extends AnyVal {
    def as[A](implicit jsonReader: JsonReader[A]): Either[ReaderError, A] = jsonReader.read(json)
  }

  implicit val stringReader: JsonReader[String] = {
    case JsonString(value) => Right(value)
    case _                 => Left(WrongType("String expected"))
  }
  implicit val intReader: JsonReader[Int] = {
    case JsonInt(value) => Right(value)
    case _              => Left(WrongType("Int expected"))
  }

  implicit val doubleReader: JsonReader[Double] = {
    case JsonDouble(value) => Right(value)
    case _                 => Left(WrongType("Double expected"))
  }

  implicit val noneReader: JsonReader[None.type] = {
    case JsonNull => Right(None)
    case _        => Left(WrongType("None type expected"))
  }

  implicit def listReader[A](implicit jsonReader: JsonReader[A]): JsonReader[List[A]] = {
    case JsonArray(values) =>
      values.map(jsonReader.read).foldLeft(Right(List.empty[A]): Either[ReaderError, List[A]]) {
        case (Right(acc), Right(value)) => Right(acc :+ value)
        case (Left(err), _)             => Left(err)
        case (_, Left(value))           => Left(value)
      }
    case _ => Left(WrongType("Json array expected"))
  }

  implicit def optionReader[A](implicit jsonReader: JsonReader[A]): JsonReader[Option[A]] = {
    case JsonNull => Right(None)
    case json     => jsonReader.read(json).map(Some(_))
  }

  def objectReader[A](f: Map[String, Json] => Either[ReaderError, A]): JsonReader[A] = {
    case JsonObject(jsonMap) => f(jsonMap)
    case _                   => Left(WrongType("Unexpected object. Expected JsonMap"))
  }

  def getOptionField[B](map: Map[String, Json], field: String)(implicit
    reader: JsonReader[B]
  ): Either[ReaderError, Option[B]] =
    map.getOrElse(field, JsonNull).as[Option[B]]

  def getField[B](map: Map[String, Json], field: String)(implicit reader: JsonReader[B]): Either[ReaderError, B] =
    map.get(field) match {
      case None    => Left(AbsentField(field))
      case Some(a) => a.as[B]
    }
}

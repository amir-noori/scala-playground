package code.playground.cats.scalawithcats

object Ch01 {

  sealed abstract class Json

  final case class JsObject(get: Map[String, Json]) extends Json

  final case class JsString(get: String) extends Json

  final case class JsInt(get: Int) extends Json

  final object JsNull extends Json

  trait JsonWriter[A] {
    def write(value: A): Json
  }

  object JsonWriterInstances {
    implicit val stringWriter: JsonWriter[String] =
      new JsonWriter[String] {
        override def write(value: String): Json = JsString(value)
      }

    implicit val personWriter: JsonWriter[Person] =
      new JsonWriter[Person] {
        override def write(person: Person): Json =
          JsObject(Map("name" -> JsString(person.name), "age" -> JsInt(person.age)))
      }
  }

  object JsonWriterSyntax {
    implicit class JsonWriterOps[A](value: A) {
      def toJson(implicit writer: JsonWriter[A]): Json = writer.write(value)
    }
  }

  object Json {

    def toJson[A](value: A)(implicit jsonWriter: JsonWriter[A]): Json =
      jsonWriter.write(value)

    def toJsonWIthPatternMatching[A](value: A): Json =
      value match {
        case person: Person =>
          JsonWriterInstances.personWriter.write(person)
        case str: String =>
          JsonWriterInstances.stringWriter.write(str)
        case _ => JsNull
      }
  }

  case class Person(name: String, age: Int)


  def main(args: Array[String]): Unit = {
    println("begin")
    import JsonWriterInstances._
    import JsonWriterSyntax._

    val p = Person("Jim", 10)
    Json.toJson(p)
    p.toJson
  }


}

object CatsApp extends App {

  import cats.Show

  val showInt = Show.apply[Int]
  println(showInt.show(100))

}
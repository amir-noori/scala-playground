package code.playground.patterns.design

object AOP extends App {

  case class Person(firstName: String, lastName: String, age: Int)

  import org.json4s._
  import org.json4s.jackson.JsonMethods._


  trait DataReader {
    def readData(): List[Person]
  }

  class DataReaderImpl extends DataReader {
    implicit val formats = DefaultFormats

    private def readUntimed(): List[Person] =
      parse("[{\"firstName\": \"Amir\", \"lastName\": \"Noori\", \"age\": 34}]").extract[List[Person]]

    override def readData(): List[Person] = readUntimed()

  }

  trait DataReaderLogging extends DataReader {
    abstract override def readData(): List[Person] = {
      println("before reading")
      val data = super.readData()
      println(s"data is: ${data}")
      println("after reading")
      data
    }
  }


  val reader = new DataReaderImpl() with DataReaderLogging
  reader.readData()



}

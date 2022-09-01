package code.playground.fp

import java.io.FileOutputStream
import java.nio.ByteBuffer

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

import scala.util.{Try, Using}

object ChannelApp extends App {

  // simple channel solution
  object SimpleChannel {

    trait Channel {
      def write(data: Any): Unit
    }

    object FileChannel extends Channel {
      override def write(data: Any): Unit = {

        val dataBytes: Array[Byte] = data match {
          case n: Int =>
            val byteBuffer = ByteBuffer.allocate(4)
            byteBuffer.putInt(n)
            byteBuffer.array()
          case s: String =>
            s.getBytes
          case _ => throw new Exception("Invalid data.")
        }

        Using(new FileOutputStream("D:\\test-file"))({ os =>
          os.write(dataBytes)
          os.flush()
        })

      }
    }

    def apply(): Unit = {
      println("writing...")
      FileChannel.write("test data...")
    }

  }

  SimpleChannel()

  ////////////////////////////////////////////////////////////////////////////////////

  object SimpleChannelImproved {

    trait ByteEncodable {
      def encode(): Array[Byte]
    }

    case class Person(firstName: String, lastName: String) extends ByteEncodable {
      override def encode(): Array[Byte] = firstName.getBytes ++ lastName.getBytes
    }

    trait Channel {
      def write(data: ByteEncodable): Unit
    }

    object FileChannel extends Channel {
      override def write(data: ByteEncodable): Unit = {
        Using(new FileOutputStream("D:\\test-file")) { os =>
          os.write(data.encode())
          os.flush()
        }
      }
    }

    def apply(): Unit = {
      println("writing...")
      FileChannel.write(Person("Mr.", "X"))
    }

  }

  SimpleChannelImproved()

  ////////////////////////////////////////////////////////////////////////////////////

  object SimpleChannelUsingTypeClass {

    trait ByteEncoder[A] {
      def encode(a: A): Array[Byte]
    }

    trait Channel {
      def write[A](data: A, encoder: ByteEncoder[A]): Unit
    }

    case class Person(firstName: String, lastName: String)

    object FileChannel extends Channel {
      override def write[A](data: A, encoder: ByteEncoder[A]): Unit = {
        Using(new FileOutputStream("D:\\test-file")) { os =>
          os.write(encoder.encode(data))
          os.flush()
        }
      }
    }

    def apply(): Unit = {
      println("writing...")
      FileChannel.write(Person("Mr.", "X"), (p: Person) => p.firstName.getBytes ++ p.lastName.getBytes)
    }

  }

  SimpleChannelUsingTypeClass()

  ////////////////////////////////////////////////////////////////////////////////////

  object SimpleChannelUsingTypeClassAndImplicits {

    trait ByteEncoder[A] {
      def encode(a: A): Array[Byte]
    }

    object ByteEncoder {

      implicit object StringByteEncoder extends ByteEncoder[String] {
        override def encode(a: String): Array[Byte] = a.getBytes
      }

    }


    trait Channel {
      def write[A](data: A)(implicit encoder: ByteEncoder[A]): Unit
    }

    case class Person(firstName: String, lastName: String)

    object FileChannel extends Channel {
      override def write[A](data: A)(implicit encoder: ByteEncoder[A]): Unit = {
        Using(new FileOutputStream("D:\\test-file")) { os =>
          os.write(encoder.encode(data))
          os.flush()
        }
      }
    }

    def apply(): Unit = {
      implicit val personByteEncoder: ByteEncoder[Person] = (p: Person) => p.firstName.getBytes ++ p.lastName.getBytes

      println("writing...")
      FileChannel.write(Person("Mr.", "X")) // implicit value is personByteEncoder
      FileChannel.write("Just a simple string.") // implicit value is StringByteEncoder
    }

  }

  SimpleChannelUsingTypeClassAndImplicits()


  ////////////////////////////////////////////////////////////////////////////////////


  object DecoderWithImplicits {

    case class Person(firstName: String, lastName: String)

    trait ByteDecoder[A] {
      def decode(dataBytes: Array[Byte]): Option[A]
    }

    // companion object to include the apply method to use implicits in the scope
    object ByteDecoder {

      def apply[A](implicit byteDecoder: ByteDecoder[A]): ByteDecoder[A] = byteDecoder

      def instance[A](f: Array[Byte] => Option[A]): ByteDecoder[A] = new ByteDecoder[A] {
        override def decode(dataBytes: Array[Byte]): Option[A] = f(dataBytes)
      }
    }

    implicit val personByteDecoder = new ByteDecoder[Person] {
      override def decode(dataBytes: Array[Byte]): Option[Person] = Some(Person("fake first name", "fake last name"))
    }

    def apply(): Unit = {
      val data: Array[Byte] = Array(100, 55, 43, 66, 45, 11)
      ByteDecoder[Person].decode(data) // sugar for ByteDecoder.apply[Person].decode(data)
    }

  }

  DecoderWithImplicits()

  ////////////////////////////////////////////////////////////////////////////////////


  object ByteCodecAndLaws {

    import org.typelevel.discipline.Laws
    import org.scalacheck.Arbitrary
    import org.scalacheck.Prop.forAll


    case class Person(firstName: String, lastName: String)


    trait ByteDecoder[A] {
      def decode(data: Array[Byte]): Option[A]
    }

    object ByteDecoder {

      def apply[A](implicit byteDecoder: ByteDecoder[A]): ByteDecoder[A] = byteDecoder

      def instance[A](f: Array[Byte] => Option[A]): ByteDecoder[A] = new ByteDecoder[A] {
        override def decode(dataBytes: Array[Byte]): Option[A] = f(dataBytes)
      }
    }

    trait ByteEncoder[A] {
      def encode(item: Option[A]): Array[Byte]
    }

    object ByteEncoder {

      def apply[A](implicit byteDecoder: ByteDecoder[A]): ByteDecoder[A] = byteDecoder

      def instance[A](f: Option[A] => Array[Byte]): ByteEncoder[A] = new ByteEncoder[A] {
        override def encode(item: Option[A]): Array[Byte] = f(item)
      }
    }


    trait ByteCodec[A] extends ByteEncoder[A] with ByteDecoder[A]

    object IntByteCodec extends ByteCodec[Int] {
      override def encode(item: Option[Int]): Array[Byte] = ByteBuffer.allocate(4).putInt(item.get).array()

      override def decode(data: Array[Byte]): Option[Int] = Some(ByteBuffer.allocate(4).put(data).flip().getInt)
    }

    object StringByteCodec extends ByteCodec[String] {
      override def encode(item: Option[String]): Array[Byte] = item.get.getBytes

      override def decode(data: Array[Byte]): Option[String] = Try(new String(data)).toOption
    }


    trait ByteCodecLaws[A] {
      //noinspection OptionEqualsSome
      def isomorphism(a: A): Boolean = codec.decode(codec.encode(Option(a))) == Some(a)
      def dummyLaw(a: A): Boolean

      def codec: ByteCodec[A]
    }

    object IntByteCodecLaws extends ByteCodecLaws[Int] {
      override def codec: ByteCodec[Int] = IntByteCodec

      override def dummyLaw(a: Int): Boolean = true
    }

    object StringByteCodecLaws extends ByteCodecLaws[String] {
      override def codec: ByteCodec[String] = StringByteCodec

      override def dummyLaw(a: String): Boolean = true
    }

    trait ByteCodecTest[A] extends Laws {
      def laws: ByteCodecLaws[A]

      def byteCodec(implicit arb: Arbitrary[A]): RuleSet = new DefaultRuleSet(
        name = "byteCodec",
        parent = None,
        "isomorphism" -> forAll(laws.isomorphism _), "dummyLaw" -> forAll(laws.dummyLaw _)
      )
    }

    object IntByteCodecTest extends ByteCodecTest[Int] {
      override def laws: ByteCodecLaws[Int] = IntByteCodecLaws
    }

    object StringByteCodecTest extends ByteCodecTest[String] {
      override def laws: ByteCodecLaws[String] = StringByteCodecLaws
    }


    def apply(): Unit = {

      class ByeCodecSpec extends AnyFunSuite with Configuration with FunSuiteDiscipline {
        checkAll("ByteCodec[Int]", IntByteCodecTest.byteCodec)
        checkAll("ByteCodec[String]", StringByteCodecTest.byteCodec)
      }

    }

  }

  ByteCodecAndLaws()


  ////////////////////////////////////////////////////////////////////////////////////





}

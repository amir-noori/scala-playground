package code.playground.patterns.traitsAndMixins

object TraitsPlayground extends App {

  trait Notifier {
    val notificationMessage: String

    def clear
  }

  /**
   * to implement Notifier we should notificationMessage as a constructor argument
   *
   * @param notificationMessage
   */
  class NotifierImpl(val notificationMessage: String) extends Notifier {
    override def clear: Unit = println("cleared")
  }

  ////////////////////////////////////////////////////////

  trait Beeper {
    def beep(times: Int) =
      1 to times foreach (i => println(s"beep: ${i}"))
  }

  // we can actually create an instance of a trait
  val beeper: Beeper = new Beeper {}
  beeper.beep(10)

  ////////////////////////////////////////////////////////


  abstract class Connector {
    def connect()

    def close()
  }

  /**
   * traits can extends classes
   */
  trait ConnectorWithDriverHelper extends Connector {
    def findDriver(): Unit = println("Driver found")
  }

  class PlSqlConnector extends ConnectorWithDriverHelper {
    override def connect(): Unit = println("connected to PL-SQL")

    override def close(): Unit = println("closed PL-SQL connection")
  }


  ////////////////////////////////////////////////////////

  trait Alarm {
    def trigger: String

    def printNotification(): Unit = println("")
  }

  class Watch(brand: String, initialTime: Long) {
    def getTime(): Long = System.currentTimeMillis() - initialTime
  }

  /**
   * expensiveWatch and cheapWatch are instances with actually extends traits anonymously.
   */
  object WatchUser {
    def main(args: Array[String]): Unit = {
      val expensiveWatch = new Watch("expensive brand", 1000L) with Alarm with Notifier {
        override def trigger: String = "The alarm was triggered."

        override def clear(): Unit = {
          System.out.println("Alarm cleared.")
        }

        override val notificationMessage: String = "Alarm is running!"
      }
      val cheapWatch = new Watch("cheap brand", 1000L) with Alarm {
        override def trigger(): String = "The alarm was triggered."
      }
      // show some watch usage.
      System.out.println(expensiveWatch.trigger)
      expensiveWatch.printNotification()
      System.out.println(s"The time is ${expensiveWatch.getTime()}.")
      expensiveWatch.clear()
      System.out.println(cheapWatch.trigger())
      System.out.println("Cheap watches cannot manually stop the alarm...")
    }
  }

  object ReallyExpensiveWatchUser {
    def main(args: Array[String]): Unit = {
      /**
       * this does not compile because Watch extends ConnectorWithDriverHelper and ConnectorWithDriverHelper extends Connector class, this means that
       * reallyExpensiveWatch is both Watch and Connector which because they are not in the same class hierarchy we get the following error:
       *
       * illegal inheritance; superclass Watch
       * is not a subclass of the superclass Connector
       * of the mixin trait ConnectorWithDriverHelper
       * val reallyExpensiveWatch = new Watch("really expensive brand", 1000L) with ConnectorWithDriverHelper {
       */
      // val reallyExpensiveWatch = new Watch("really expensive brand", 1000L) with ConnectorWithDriverHelper {
      //        override def connect(): Unit = {
      //          System.out.println("Connected with another connector.")
      //        }
      //
      //        override def close(): Unit = {
      //          System.out.println("Closed with another connector.")
      //        }
      //      }
      //      System.out.println("Using the really expensive watch.")
      //      reallyExpensiveWatch.findDriver()
      //      reallyExpensiveWatch.connect()
      //      reallyExpensiveWatch.close()
    }
  }

  ////////////////////////////////////////////////////////

  trait User {
    def username: String
  }

  trait Tweeter {
    this: User => // reassign this

    def tweet(tweetText: String) = println(s"$username: $tweetText")
  }

  /**
   * if VerifiedTweeter extends Tweeter then it must also extends User as well because of self type in Tweeter trait.
   *
   * @param username_
   */
  class VerifiedTweeter(val username_ : String) extends Tweeter with User { // We mixin User because Tweeter required it
    def username = s"real $username_"
  }

  ////////////////////////////////////////////////////////

  trait FormalGreeting {
    def greet(name: String): Unit
  }

  trait InformalGreeting {
    def greet(name: String): Unit
  }

  /**
   * clashing traits
   */
  class Person extends FormalGreeting with InformalGreeting {
    override def greet(name: String): Unit = println(s"Hi there ${name}")
  }


  ////////////////////////////////////////////////////////

  trait A {
    def hello(): String = "Hello, I am trait A!"
  }
  trait B {
    def hello(): String = "Hello, I am trait B!"
  }

  /**
   * How to fix clashing traits
   */
  class Clashing extends A with B {
    // will call A hello
    override def hello(): String = super[A].hello()
    // will call B hello
    def hello2(): String = super[B].hello()
    // will call B hello
    def hello3(): String = super.hello()
  }

  ////////////////////////////////////////////////////////







}



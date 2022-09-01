package code.playground.akka.actor

import akka.actor.typed.{ActorSystem, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}

class SimpleActor {

  trait TestMessage

  case class TestMessage1(m: String) extends TestMessage

  case class TestMessage2(m: String) extends TestMessage

  object TestActor {
    def apply(): Behavior[TestMessage] = Behaviors.setup[TestMessage] { context =>
      start(context, TestMessage1("do begin"))
    }

    def start(context: ActorContext[TestMessage], state: TestMessage): Behavior[TestMessage] = {
      Behaviors.receiveMessage {
        case TestMessage1(m) =>
          println(s"Test B1 with value: $m")
          println(s"current state: $state")
          start(context, TestMessage2("Message two"))
        case TestMessage2(m) =>
          println(s"Test B2 with value: $m")
          println(s"current state: $state")
          start(context, TestMessage2("Message one"))
        case _ =>
          Behaviors.same
      }
    }

  }

  def main(args: Array[String]): Unit = {
    val myActor: ActorSystem[TestMessage] = ActorSystem[TestMessage](TestActor(), "Test-System")
    myActor ! TestMessage1("here we go")
    myActor ! TestMessage2("here we go again")
    myActor ! TestMessage1("and again")
  }

}

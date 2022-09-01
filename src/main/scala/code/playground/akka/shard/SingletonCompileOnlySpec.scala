package code.playground.akka.shard

import com.typesafe.config.{Config, ConfigFactory}

object SingletonCompileOnlySpec extends App {

  import akka.actor.typed.{ ActorRef, ActorSystem, Behavior, SupervisorStrategy }
  import akka.actor.typed.scaladsl.Behaviors
  import scala.concurrent.duration._

  import akka.cluster.typed.ClusterSingletonSettings

  val system = ActorSystem[Nothing](Behaviors.empty, "Singleton", getConfig())

  private def getConfig(): Config =
    ConfigFactory.parseString(s"""
    akka.cluster.singleton {
        singleton-name = "singleton"
    }

    akka {
        actor {
          provider = cluster
        }
    }

  """).withFallback(ConfigFactory.load())


  //#counter
  object Counter {
    sealed trait Command
    case object Increment extends Command
    final case class GetValue(replyTo: ActorRef[Int]) extends Command
    case object GoodByeCounter extends Command

    def apply(): Behavior[Command] = {
      println("")
      def updated(value: Int): Behavior[Command] = {
        Behaviors.receiveMessage[Command] {
          case Increment =>
            updated(value + 1)
          case GetValue(replyTo) =>
            replyTo ! value
            Behaviors.same
          case GoodByeCounter =>
            // Possible async action then stop
            Behaviors.stopped
        }
      }

      updated(0)
    }
  }
  //#counter

  //#singleton
  import akka.cluster.typed.ClusterSingleton
  import akka.cluster.typed.SingletonActor

  val singletonManager = ClusterSingleton(system)
  // Start if needed and provide a proxy to a named singleton
  val proxy: ActorRef[Counter.Command] = singletonManager.init(
    SingletonActor(Behaviors.supervise(Counter()).onFailure[Exception](SupervisorStrategy.restart), "GlobalCounter"))

  proxy ! Counter.Increment
  //#singleton

  //#stop-message
  val singletonActor = SingletonActor(Counter(), "GlobalCounter").withStopMessage(Counter.GoodByeCounter)
  singletonManager.init(singletonActor)
  //#stop-message

  //#backoff
  val proxyBackOff: ActorRef[Counter.Command] = singletonManager.init(
    SingletonActor(
      Behaviors
        .supervise(Counter())
        .onFailure[Exception](SupervisorStrategy.restartWithBackoff(1.second, 10.seconds, 0.2)),
      "GlobalCounter"))
  //#backoff

  //#create-singleton-proxy-dc
  val singletonProxy: ActorRef[Counter.Command] = ClusterSingleton(system).init(
    SingletonActor(Counter(), "GlobalCounter").withSettings(ClusterSingletonSettings(system).withDataCenter("dc2")))
  //#create-singleton-proxy-dc
}

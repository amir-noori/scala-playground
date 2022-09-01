package code.playground.akka.shard

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.cluster.sharding.typed.ShardingEnvelope
import akka.cluster.sharding.typed.scaladsl.{ClusterSharding, Entity, EntityRef, EntityTypeKey}
import akka.util.Timeout
import SimpleShardApp.Counter.Command
import code.playground.akka.serialization.CborSerializable
import com.typesafe.config.{Config, ConfigFactory}

import scala.concurrent.duration.DurationInt

object SimpleShardApp extends App {


  private def getConfig(): Config =
    ConfigFactory.parseString(
      s"""
    akka {

        loglevel = DEBUG

        actor {
          provider = cluster
          jackson-json = "akka.serialization.jackson.JacksonJsonSerializer"
        }

        remote.artery {
          canonical.port = 2554
          canonical.hostname = 127.0.0.1
        }

        serialization-bindings {
          "com.behsa.mediation.playground.akka.shard.CborSerializable" = jackson-json
        }

        cluster {
          seed-nodes = [
            "akka://my-system@127.0.0.1:2554",
            "akka://my-system@127.0.0.1:2555",
            "akka://my-system@127.0.0.1:2556"
          ]

          sharding {
            number-of-shards = 100
          }

          downing-provider-class = "akka.cluster.sbr.SplitBrainResolverProvider"
        }

    }

  """).withFallback(ConfigFactory.load())


  object Counter {
    sealed trait Command extends CborSerializable

    case object Increment extends Command

    final case class GetValue(replyTo: ActorRef[Int]) extends Command

    def apply(entityId: String): Behavior[Command] = Behaviors.setup { context =>
      context.log.debug("starting counter actor.")
      updated(0)
    }

    def updated(value: Int): Behavior[Command] = {
      Behaviors.receiveMessage[Command] {
        case Increment =>
          println(s"current value is: $value. incrementing...")
          updated(value + 1)
        case GetValue(replyTo) =>
          println(s"replying to $replyTo the current value: $value")
          replyTo ! value
          Behaviors.same
      }
    }
  }

  object Guardian {

    def apply(): Behavior[Nothing] = Behaviors.setup[Nothing] { context =>
      val sharding = ClusterSharding(context.system)

      val TypeKey = EntityTypeKey[Command]("Counter")

      val shardRegion: ActorRef[ShardingEnvelope[Command]] =
        sharding.init(Entity(TypeKey)(entityContext => Counter(entityContext.entityId)))

      val counterOne: EntityRef[Command] = sharding.entityRefFor(TypeKey, "counter-1")

      implicit val timeout: Timeout = 10.millis
      counterOne ! Counter.Increment
      counterOne.ask(Counter.GetValue)

      // Entity id is specified via an `ShardingEnvelope`
      shardRegion ! ShardingEnvelope("counter-1", Counter.Increment)

      Behaviors.empty
    }

  }

  ActorSystem[Nothing](Guardian(), "my-system", getConfig())

}

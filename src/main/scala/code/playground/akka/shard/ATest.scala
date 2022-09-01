package code.playground.akka.shard

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.cluster.sharding.typed.scaladsl.{ClusterSharding, Entity, EntityTypeKey}
import akka.persistence.typed.PersistenceId
import BlogPostEntity.Command
import com.typesafe.config.{Config, ConfigFactory}

import scala.annotation.nowarn
import scala.concurrent.duration._

object ATest extends App {

  val system = ActorSystem[Nothing](Behaviors.empty, "Sharding", getConfig())

  private def getConfig(): Config =
    ConfigFactory.parseString(s"""
    akka {
        actor {
          provider = cluster
          jackson-json = "akka.serialization.jackson.JacksonJsonSerializer"

        }
         serialization-bindings {
          "com.behsa.mediation.playground.akka.shard.CborSerializable" = jackson-json
        }
    }

  """).withFallback(ConfigFactory.load())

  //#sharding-extension
  import akka.cluster.sharding.typed.ShardingEnvelope
  import akka.cluster.sharding.typed.scaladsl.ClusterSharding
  import akka.cluster.sharding.typed.scaladsl.EntityTypeKey
  import akka.cluster.sharding.typed.scaladsl.EntityRef

  val sharding = ClusterSharding(system)
  //#sharding-extension

  //#counter
  object Counter {
    sealed trait Command
    case object Increment extends Command
    final case class GetValue(replyTo: ActorRef[Int]) extends Command

    def apply(entityId: String): Behavior[Command] = {

      println("aaa")

      def updated(value: Int): Behavior[Command] = {
        Behaviors.receiveMessage[Command] {
          case Increment =>
            println("bbb")
            updated(value + 1)
          case GetValue(replyTo) =>
            println("ccc")
            replyTo ! value
            Behaviors.same
        }
      }

      updated(0)

    }
  }
  //#counter

  //#init
  val TypeKey = EntityTypeKey[Counter.Command]("Counter")

  val shardRegion: ActorRef[ShardingEnvelope[Counter.Command]] =
    sharding.init(Entity(TypeKey)(createBehavior = entityContext => Counter(entityContext.entityId)))
  //#init

  //#send
  // With an EntityRef
  val counterOne: EntityRef[Counter.Command] = sharding.entityRefFor(TypeKey, "counter-1")
  counterOne ! Counter.Increment

  // Entity id is specified via an `ShardingEnvelope`
  shardRegion ! ShardingEnvelope("counter-1", Counter.Increment)
  //#send

  //#persistence
  val BlogTypeKey = EntityTypeKey[Command]("BlogPost")

  ClusterSharding(system).init(Entity(BlogTypeKey) { entityContext =>
    BlogPostEntity(entityContext.entityId, PersistenceId(entityContext.entityTypeKey.name, entityContext.entityId))
  })
  //#persistence

  //#roles
  sharding.init(
    Entity(TypeKey)(createBehavior = entityContext => Counter(entityContext.entityId)).withRole("backend"))
  //#roles

}

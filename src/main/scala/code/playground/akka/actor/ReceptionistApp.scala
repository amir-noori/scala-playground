package code.playground.akka.actor

object ReceptionistApp extends App {

  import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
  import akka.actor.typed.scaladsl.Behaviors
  import akka.actor.typed.{ActorRef, Behavior}


  object PingService {

    final case class Ping(replyTo: ActorRef[Pong.type])

    case object Pong

    val pingServiceKey: ServiceKey[Ping] = ServiceKey[Ping]("pingService")

    def apply(): Behavior[Ping] = Behaviors.setup { context =>
      context.system.receptionist ! Receptionist.register(pingServiceKey, context.self)
      println("PingService started")
      Behaviors.receiveMessage {
        case Ping(replyTo) =>
          println(s"Pinged from $replyTo")
          replyTo ! Pong
          Behaviors.same
      }
    }

  }

  object Pinger {

    import PingService.{Ping, Pong}

    def apply(pingService: ActorRef[PingService.Ping]): Behavior[Pong.type] = {
      Behaviors.setup { context =>
        println("Pinger started")

        pingService ! Ping(context.self)
        Behaviors.receiveMessage {
          case Pong =>
            println("ponged")
            Behaviors.stopped
          case _ =>
            Behaviors.stopped
        }
      }
    }

  }

  object Guardian {
    def apply(): Behavior[Receptionist.Listing] = {
      Behaviors
        .setup[Receptionist.Listing] { context =>
          println("guardian started")
          context.system.receptionist ! Receptionist.Subscribe(PingService.pingServiceKey, context.self)

          Behaviors.receiveMessagePartial[Receptionist.Listing] {
            case PingService.pingServiceKey.Listing(listings) =>
              println(s"guardian listing: $listings")
              listings.foreach(ps => context.spawnAnonymous(Pinger(ps)))
              Behaviors.same
          }
        }
        .narrow
    }
  }


  object PingManager {
    sealed trait Command

    case object PingAll extends Command

    private case class ListingResponse(listing: Receptionist.Listing) extends Command

    def apply(): Behavior[Command] = {
      Behaviors.setup[Command] { context =>
        val listingResponseAdapter = context.messageAdapter[Receptionist.Listing](ListingResponse.apply)

        context.spawnAnonymous(PingService())

        Behaviors.receiveMessagePartial {
          case PingAll =>
            context.system.receptionist ! Receptionist.Find(PingService.pingServiceKey, listingResponseAdapter)
            Behaviors.same
          case ListingResponse(PingService.pingServiceKey.Listing(listings)) =>
            listings.foreach(ps => context.spawnAnonymous(Pinger(ps)))
            Behaviors.same
        }
      }
    }
  }

  import akka.actor.typed.ActorSystem

  //  val pingActor = ActorSystem[PingService.Ping](PingManager(), "PingManager")
  val guardianActor = ActorSystem[Receptionist.Listing](Guardian(), "guardianActor")
  guardianActor.receptionist ! Receptionist.Subscribe(PingService.pingServiceKey, guardianActor)

  guardianActor.systemActorOf(PingService(), "pingService1")
  guardianActor.systemActorOf(PingService(), "pingService2")


}

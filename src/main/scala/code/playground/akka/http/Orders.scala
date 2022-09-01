package code.playground.akka.http

import akka.actor.typed.{ActorRef, ActorSystem}
import akka.actor.typed.scaladsl.Behaviors
import akka.Done
import akka.actor.Status.Success
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import spray.json.DefaultJsonProtocol.{jsonFormat1, jsonFormat2}
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.stream.scaladsl.Source
import akka.util.{ByteString, Timeout}
import spray.json.DefaultJsonProtocol._
import akka.actor.typed.scaladsl.AskPattern._

import scala.concurrent.duration.DurationInt
import scala.concurrent.Future
import scala.util.Random

object Orders extends App {


    implicit val system = ActorSystem(Behaviors.empty, "order-system")
    implicit val executionContext = system.executionContext

    implicit val itemFormat = jsonFormat2(Item)
    implicit val orderFormat = jsonFormat1(Order)


    var orders: List[Item] = Nil

    final case class Item(name: String, id: Long)

    final case class Order(items: List[Item])

    def fetchItem(itemId: Long): Future[Option[Item]] = Future {
      orders.find(item => item.id == itemId)
    }

    def saveOrder(order: Order): Future[Done] = {
      orders = order match {
        case Order(items) => orders ::: items
        case _ => orders
      }
      Future {
        Done
      }
    }


    val route: Route =
      concat(
        get {
          pathPrefix("item" / LongNumber) { itemId =>
            val itemOption: Future[Option[Item]] = fetchItem(itemId)
            onSuccess(itemOption) {
              case Some(item) => complete(item)
              case None => complete(StatusCodes.NotFound)
            }
          }
        },
        post {
          path("create-order") {
            entity(as[Order]) { order =>
              val savedOrderResult: Future[Done] = saveOrder(order)
              onSuccess(savedOrderResult) {
                case _ => complete("order created")
              }
            }
          }
        }
      )


    val statusRoute = path("status") {
      get {
        println("received HTTP GET request.")
        complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
          """
            <h1 style='color: green'>Server Is Running...</h1>
          """.stripMargin))
      }
    }

    ////////////////////////////////////////////////////////////////////////


    val numbers = Source.fromIterator(() =>
      Iterator.continually(Random.nextInt()))

    val randomNumberStreamRoute =
      path("random") {
        get {
          complete(
            HttpEntity(
              ContentTypes.`text/plain(UTF-8)`,
              // transform each number to a chunk of bytes
              numbers.map(n => ByteString(s"$n\n"))
            )
          )
        }
      }


  ////////////////////////////////////////////////////////////////////////


  object Auction {

    sealed trait Message

    case class Bid(userId: String, offer: Int) extends Message

    case class GetBids(replyTo: ActorRef[Bids]) extends Message

    case class Bids(bids: List[Bid])

    def apply: Behaviors.Receive[Message] = apply(List.empty)

    def apply(bids: List[Bid]): Behaviors.Receive[Message] = Behaviors.receive {
      case (ctx, bid@Bid(userId, offer)) =>
        ctx.log.info(s"Bid complete: $userId, $offer")
        apply(bids :+ bid)
      case (_, GetBids(replyTo)) =>
        replyTo ! Bids(bids)
        Behaviors.same
    }

  }


  import Auction._

  implicit val bidFormat = jsonFormat2(Auction.Bid)
  implicit val bidsFormat = jsonFormat1(Auction.Bids)

  implicit val actorSystem: ActorSystem[Auction.Message] = ActorSystem(Auction.apply, "auction")
  implicit val auction: ActorRef[Auction.Message] = actorSystem

  val auctionRoute =
    path("auction") {
      concat(
        put {
          parameters("bid".as[Int], "user") { (bid, user) =>
            // place a bid, fire-and-forget
            auction ! Bid(user, bid)
            complete(StatusCodes.Accepted, "bid placed")
          }
        },
        get {
          implicit val timeout: Timeout = 5.seconds

          // query the actor for the current auction state
          val bids: Future[Bids] = (auction ? GetBids).mapTo[Bids]
          complete(bids)
        }
      )
    }


  ////////////////////////////////////////////////////////////////////////

  /*
  * to create orders:
  * curl -H "Content-Type: application/json" -X POST -d '{"items":[{"name":"first Item","id":42}, {"name":"second Item","id":57}]}' http://localhost:8822/create-order
  *
  * to get item from order list
  * curl http://localhost:8822/item/42
  * */
  Http().newServerAt("localhost", 8822).bind(route ~ statusRoute)

  Http().newServerAt("localhost", 8833).bind(randomNumberStreamRoute)

  Http().newServerAt("localhost", 8844).bind(auctionRoute)


}

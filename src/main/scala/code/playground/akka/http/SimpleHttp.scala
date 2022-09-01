package code.playground.akka.http

import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}

object SimpleHttp extends App {

  import akka.actor.typed.ActorSystem
  import akka.http.scaladsl.server.Directives._

  implicit val system = ActorSystem(Behaviors.empty, "my-system")

  val route = path("hello") {
    get {
      println("received HTTP GET request.")
      complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
        """
          <h1>Welcome</h1>
          <p style='color: blue'>
            This is just a test response.
          </p>
          """.stripMargin))
    }
  }

  val route2 = path("hello2") {
    get {
      println("received HTTP GET request.")
      complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
        """
          <h1>Welcome</h1>
          <p style='color: blue'>
            This is just a test response2.
          </p>
          """.stripMargin))
    }
  }

  Http().newServerAt("localhost", 8822).bind(route ~ route2)


}

package scorex.api.http

import akka.actor.ActorRefFactory
import spray.http.MediaTypes._
import spray.httpx.marshalling.ToResponseMarshallable
import spray.routing._

trait ApiRoute extends HttpService {
  val context: ActorRefFactory
  val route: spray.routing.Route

  def actorRefFactory = context

  def jsonRoute(fn: => ToResponseMarshallable, method: Directive0 = get) = method {
    respondWithMediaType(`application/json`) {
      complete(
        fn
      )
    }
  }

}

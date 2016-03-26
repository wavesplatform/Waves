package scorex.api.http

import akka.actor.ActorRefFactory
import scorex.app.Application
import spray.http.HttpHeaders.RawHeader
import spray.http.MediaTypes._
import spray.httpx.marshalling.ToResponseMarshallable
import spray.routing._

trait ApiRoute extends HttpService {
  val application: Application
  val context: ActorRefFactory
  val route: Route

  lazy val corsAllowed = application.settings.corsAllowed

  def actorRefFactory: ActorRefFactory = context

  def jsonRoute(fn: => ToResponseMarshallable, method: Directive0 = get): Route = method {
    //TODO use incompletedJsonRoute after permacoin-consensus release
    val jsonResponse = respondWithMediaType(`application/json`) {
      complete(fn)
    }

    if (corsAllowed) respondWithHeader(RawHeader("Access-Control-Allow-Origin", "*"))(jsonResponse)
    else jsonResponse
  }


  def incompletedJsonRoute(fn: => Route, method: Directive0 = get): Route = method {
    val jsonResponse = respondWithMediaType(`application/json`) {
      fn
    }

    if (corsAllowed) respondWithHeader(RawHeader("Access-Control-Allow-Origin", "*"))(jsonResponse)
    else jsonResponse
  }

}
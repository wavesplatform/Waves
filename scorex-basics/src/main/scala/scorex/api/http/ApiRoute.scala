package scorex.api.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server.{Directive0, Route}
import akka.http.scaladsl.server.Directives._
import akka.actor.ActorRefFactory
import scorex.app.Application

trait ApiRoute {
  val application: Application
  val context: ActorRefFactory
  val route: Route

  lazy val corsAllowed = application.settings.corsAllowed

  def actorRefFactory: ActorRefFactory = context

  def jsonRoute(fn: => ToResponseMarshallable, method: Directive0 = get): Route = method {
    incompletedJsonRoute(complete(fn), method)
  }


  def incompletedJsonRoute(fn: => Route, method: Directive0 = get): Route = method {
    // TODO XXX
    //    val jsonResponse = respondWithMediaType(`application/json`) {
    val jsonResponse = fn
    //    }

    if (corsAllowed) respondWithHeader(RawHeader("Access-Control-Allow-Origin", "*"))(jsonResponse)
    else jsonResponse
  }

}
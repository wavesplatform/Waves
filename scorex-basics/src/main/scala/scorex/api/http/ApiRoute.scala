package scorex.api.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.{ContentType, MediaTypes}
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server.{Directives, Directive0, Route}
import akka.actor.ActorRefFactory
import scorex.app.Application

trait ApiRoute extends Directives {
  val application: Application
  val context: ActorRefFactory
  val route: Route

  lazy val corsAllowed = application.settings.corsAllowed

  def actorRefFactory: ActorRefFactory = context

  def jsonRoute(fn: => ToResponseMarshallable, method: Directive0 = get): Route = method {
    incompletedJsonRoute(complete(fn), method)
  }


  def incompletedJsonRoute(fn: => Route, method: Directive0 = get): Route = method {
    if (corsAllowed) respondWithHeaders(RawHeader("Content-Type", "application/json"), RawHeader("Access-Control-Allow-Origin", "*"))(fn)
    else respondWithHeaders(RawHeader("Content-Type", "application/json"))(fn)
  }

}
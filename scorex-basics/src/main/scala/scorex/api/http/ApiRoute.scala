package scorex.api.http

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.{Directive0, Directives, Route}
import akka.util.Timeout
import play.api.libs.json.JsValue
import scorex.app.Application

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

trait ApiRoute extends Directives {
  val application: Application
  val context: ActorRefFactory
  val route: Route

  implicit val timeout = Timeout(5.seconds)

  lazy val corsAllowed = application.settings.corsAllowed

  def actorRefFactory: ActorRefFactory = context

  def getJsonRoute(fn: Future[JsValue]): Route = jsonRoute(Await.result(fn, timeout.duration), get)

  def getJsonRoute(fn: JsValue): Route = jsonRoute(fn, get)

  def postJsonRoute(fn: JsValue): Route = jsonRoute(fn, post)

  def jsonRoute(fn: JsValue, method: Directive0): Route = method {
    val resp = complete(HttpEntity(ContentTypes.`application/json`, fn.toString()))
    if (corsAllowed) respondWithHeader(RawHeader("Access-Control-Allow-Origin", "*"))(resp)
    else resp
  }


  def incompletedJsonRoute(fn: => Route, method: Directive0 = get): Route = method {
    if (corsAllowed) respondWithHeaders(RawHeader("Content-Type", "application/json"), RawHeader("Access-Control-Allow-Origin", "*"))(fn)
    else respondWithHeaders(RawHeader("Content-Type", "application/json"))(fn)
  }

}
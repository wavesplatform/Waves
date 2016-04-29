package scorex.api.http

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.{Directive0, Directives, Route}
import akka.util.Timeout
import play.api.libs.json.JsValue
import scorex.app.Application
import scorex.crypto.hash.CryptographicHash.Digest
import scorex.crypto.hash.SecureCryptographicHash

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

trait ApiRoute extends Directives with CommonApiFunctions {
  val application: Application
  val context: ActorRefFactory
  val route: Route

  implicit val timeout = Timeout(5.seconds)

  lazy val corsAllowed = application.settings.corsAllowed
  lazy val apiKeyHash = application.settings.apiKeyHash

  def actorRefFactory: ActorRefFactory = context

  def getJsonRoute(fn: Future[JsValue]): Route =
    jsonRoute(Await.result(fn, timeout.duration), get)

  def getJsonRoute(fn: JsValue): Route = jsonRoute(fn, get)

  def postJsonRoute(fn: JsValue): Route = jsonRoute(fn, post)

  def postJsonRoute(fn: Future[JsValue]): Route = jsonRoute(Await.result(fn, timeout.duration), post)

  def deleteJsonRoute(fn: JsValue): Route = jsonRoute(fn, delete)

  def deleteJsonRoute(fn: Future[JsValue]): Route = jsonRoute(Await.result(fn, timeout.duration), delete)

  private def jsonRoute(fn: JsValue, method: Directive0): Route = method {
    val resp = complete(HttpEntity(ContentTypes.`application/json`, fn.toString()))
    withCors(resp)
  }

  def withCors(fn: => Route): Route = {
    if (corsAllowed) respondWithHeaders(RawHeader("Access-Control-Allow-Origin", "*"))(fn)
    else fn
  }

  def withAuth(route: => Route): Route = {
    optionalHeaderValueByName("api_key") { case keyOpt =>
      if (isValid(keyOpt)) route
      else complete(HttpEntity(ContentTypes.`application/json`, ApiKeyNotValid.json.toString()))
    }
  }

  private def isValid(keyOpt: Option[String]): Boolean = {
    lazy val keyHash: Option[Digest] = keyOpt.map(SecureCryptographicHash(_))
    (apiKeyHash, keyHash) match {
      case (None, _) => true
      case (Some(expected), Some(passed)) => expected sameElements passed
      case _ => false
    }
  }

}
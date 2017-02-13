package scorex.api.http

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCode}
import akka.http.scaladsl.server._
import akka.util.Timeout
import com.wavesplatform.settings.RestAPISettings
import play.api.libs.json.JsValue
import scorex.crypto.encode.Base58
import scorex.crypto.hash.SecureCryptographicHash

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

final case class JsonResponse(response: JsValue, code: StatusCode)

trait ApiRoute extends Directives with CommonApiFunctions {
  val settings: RestAPISettings
  val context: ActorRefFactory
  val route: Route

  implicit val timeout = Timeout(5.seconds)

  lazy val corsAllowed = settings.cors
  lazy val apiKeyHash = Base58.decode(settings.apiKeyHash).toOption

  //def actorRefFactory: ActorRefFactory = context

  def getJsonRoute(fn: Future[JsonResponse]): Route =
    jsonRoute(Await.result(fn, timeout.duration), get)

  def getJsonRoute(fn: JsonResponse): Route = jsonRoute(fn, get)

  def postJsonRoute(fn: JsonResponse): Route = jsonRoute(fn, post)

  def postJsonRoute(fn: Future[JsonResponse]): Route = jsonRoute(Await.result(fn, timeout.duration), post)

  def deleteJsonRoute(fn: JsonResponse): Route = jsonRoute(fn, delete)

  def deleteJsonRoute(fn: Future[JsonResponse]): Route = jsonRoute(Await.result(fn, timeout.duration), delete)

  def jsonRouteAsync(fn: Future[JsonResponse]): Route = {
    onSuccess(fn) { res: JsonResponse =>
      complete(res.code -> HttpEntity(ContentTypes.`application/json`, res.response.toString))
    }
  }

  val Base58String: PathMatcher1[Array[Byte]] =
    PathMatcher("""(\w+)""".r).flatMap { s => Base58.decode(s).toOption }

  private def jsonRoute(fn: JsonResponse, method: Directive0): Route = method {
    val resp = complete(fn.code -> HttpEntity(ContentTypes.`application/json`, fn.response.toString))
    withCors(resp)
  }

  def withAuth(route: => Route): Route = {
    optionalHeaderValueByName("api_key") { case keyOpt =>
      if (isValid(keyOpt)) route
      else {
        val resp = complete(ApiKeyNotValid.code -> HttpEntity(ContentTypes.`application/json`, ApiKeyNotValid.json.toString))
        withCors(resp)
      }
    }
  }

  private def withCors(fn: => Route): Route = {
    if (corsAllowed) respondWithHeaders(RawHeader("Access-Control-Allow-Origin", "*"))(fn)
    else fn
  }

  private def isValid(keyOpt: Option[String]): Boolean = {
    lazy val keyHash = keyOpt.map(SecureCryptographicHash(_))
    (apiKeyHash, keyHash) match {
      case (None, _) => true
      case (Some(expected), Some(passed)) => expected sameElements passed
      case _ => false
    }
  }

}
package scorex.api.http

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import akka.http.scaladsl.marshalling.{ToResponseMarshallable, ToResponseMarshaller}
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCode}
import akka.http.scaladsl.server._
import akka.util.Timeout
import com.wavesplatform.http.PlayJsonSupport._
import com.wavesplatform.settings.RestAPISettings
import play.api.libs.json.{JsValue, Reads, Writes}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.SecureCryptographicHash
import scorex.transaction.{Transaction, ValidationError}

final case class JsonResponse(response: JsValue, code: StatusCode)

trait ApiRoute extends Directives with CommonApiFunctions {
  val settings: RestAPISettings
  val route: Route

  implicit val timeout = Timeout(5.seconds)

  lazy val corsAllowed = settings.cors
  lazy val apiKeyHash = Base58.decode(settings.apiKeyHash).toOption

  type TRM[A] = ToResponseMarshaller[A]

  import akka.http.scaladsl.marshalling.PredefinedToResponseMarshallers._
  implicit val aem: TRM[ApiError] = fromStatusCodeAndValue[StatusCode, JsValue].compose { ae => ae.code -> ae.json }
  implicit val vem: TRM[ValidationError] = aem.compose(ve => ApiError.fromValidationError(ve))

  implicit val tw: Writes[Transaction] = Writes(_.json)

  def process[A: Reads](f: A => ToResponseMarshallable) = entity(as[A]) { a =>
    complete(f(a))
  }

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

  def withAuth: Directive0 = apiKeyHash.fold(pass) { hashFromSettings =>
    optionalHeaderValueByName("api_key").flatMap {
      case Some(apiKey) if SecureCryptographicHash(apiKey) sameElements hashFromSettings => pass
      case other => complete(ApiKeyNotValid)
    }
  }

  def withCors: Directive0 = if (corsAllowed) respondWithHeader(`Access-Control-Allow-Origin`.*) else pass
}

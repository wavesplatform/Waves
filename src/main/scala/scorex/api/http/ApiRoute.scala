package scorex.api.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCode
import akka.http.scaladsl.server.{Directive0, Directives, Route}
import com.wavesplatform.http.PlayJsonSupport
import com.wavesplatform.settings.RestAPISettings
import play.api.libs.json.{JsValue, Reads}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.SecureCryptographicHash

final case class JsonResponse(response: JsValue, code: StatusCode)

trait ApiRoute extends Directives with CommonApiFunctions with PlayJsonSupport {
  val settings: RestAPISettings
  val route: Route

  lazy val corsAllowed = settings.cors
  lazy val apiKeyHash = Base58.decode(settings.apiKeyHash).toOption

  def json[A: Reads](f: A => ToResponseMarshallable) = entity(as[A]) { a =>
    complete(f(a))
  }

  def withAuth: Directive0 = apiKeyHash.fold(pass) { hashFromSettings =>
    optionalHeaderValueByName("api_key").flatMap {
      case Some(apiKey) if SecureCryptographicHash(apiKey) sameElements hashFromSettings => pass
      case _ => complete(ApiKeyNotValid)
    }
  }
}

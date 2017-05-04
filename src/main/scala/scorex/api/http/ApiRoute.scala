package scorex.api.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server._
import com.wavesplatform.http.{ApiMarshallers, PlayJsonException, api_key}
import com.wavesplatform.settings.RestAPISettings
import play.api.libs.json.Reads
import scorex.crypto.encode.Base58
import scorex.crypto.hash.SecureCryptographicHash


trait ApiRoute extends Directives with CommonApiFunctions with ApiMarshallers {
  val settings: RestAPISettings
  val route: Route

  private lazy val apiKeyHash = Base58.decode(settings.apiKeyHash).toOption

  private val jsonRejectionHandler = RejectionHandler.newBuilder().handle {
    case ValidationRejection(_, Some(PlayJsonException(cause, errors))) => complete(WrongJson(cause, errors))
  }.result()

  def json[A: Reads](f: A => ToResponseMarshallable): Route = handleRejections(jsonRejectionHandler) {
    entity(as[A]) { a => complete(f(a)) }
  }

  def withAuth: Directive0 = apiKeyHash.fold(pass) { hashFromSettings =>
    optionalHeaderValueByType[api_key](()).flatMap {
      case Some(k) if SecureCryptographicHash(k.key).sameElements(hashFromSettings) => pass
      case _ => complete(ApiKeyNotValid)
    }
  }

  def processRequest[A: Reads](pathMatcher: String, f: A => ToResponseMarshallable): Route =
    (path(pathMatcher) & post & withAuth) {
      json[A](f)
    }
}

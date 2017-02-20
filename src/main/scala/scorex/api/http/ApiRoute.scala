package scorex.api.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.{Directive0, Directives, Route}
import com.wavesplatform.http.ApiMarshallers
import com.wavesplatform.settings.RestAPISettings
import play.api.libs.json.Reads
import scorex.crypto.encode.Base58
import scorex.crypto.hash.SecureCryptographicHash


trait ApiRoute extends Directives with CommonApiFunctions with ApiMarshallers {
  val settings: RestAPISettings
  val route: Route

  private lazy val apiKeyHash = Base58.decode(settings.apiKeyHash).toOption

  def json[A: Reads](f: A => ToResponseMarshallable): Route = entity(as[A]) { a =>
    complete(f(a))
  }

  def withAuth: Directive0 = apiKeyHash.fold(pass) { hashFromSettings =>
    optionalHeaderValueByName("api_key").flatMap {
      case Some(apiKey) if SecureCryptographicHash(apiKey) sameElements hashFromSettings => pass
      case _ => complete(ApiKeyNotValid)
    }
  }
}

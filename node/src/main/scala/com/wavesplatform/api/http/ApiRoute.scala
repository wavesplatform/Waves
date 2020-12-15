package com.wavesplatform.api.http

import akka.http.scaladsl.server._
import com.wavesplatform.api.http.ApiError.ApiKeyNotValid
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.utils._

trait ApiRoute extends Directives with CustomDirectives with ApiMarshallers with ScorexLogging {
  def route: Route
}

trait AuthRoute { this: ApiRoute =>
  def settings: RestAPISettings

  protected lazy val apiKeyHash: Option[Array[Byte]] = Base58.tryDecode(settings.apiKeyHash).toOption

  def withAuth: Directive0 = apiKeyHash.fold[Directive0](complete(ApiKeyNotValid)) { hashFromSettings =>
    optionalHeaderValueByType[`X-Api-Key`](()).flatMap {
      case Some(k) if java.util.Arrays.equals(crypto.secureHash(k.value.utf8Bytes), hashFromSettings) => pass
      case _ =>
        optionalHeaderValueByType[api_key](()).flatMap {
          case Some(k) if java.util.Arrays.equals(crypto.secureHash(k.value.utf8Bytes), hashFromSettings) => pass
          case _                                                                                          => complete(ApiKeyNotValid)
        }
    }
  }
}

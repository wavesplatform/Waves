package com.wavesplatform.api.http

import java.util.NoSuchElementException
import java.util.concurrent.ExecutionException

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server._
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto
import com.wavesplatform.http.{ApiMarshallers, PlayJsonException, api_key, deprecated_api_key}
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.ValidationError.GenericError
import play.api.libs.json.{JsResultException, Reads}

trait ApiRoute extends Directives with CommonApiFunctions with ApiMarshallers {
  val settings: RestAPISettings
  val route: Route

  private lazy val apiKeyHash = Base58.tryDecodeWithLimit(settings.apiKeyHash).toOption

  private val jsonRejectionHandler = RejectionHandler
    .newBuilder()
    .handle { case ValidationRejection(_, Some(PlayJsonException(cause, errors))) => complete(WrongJson(cause, errors)) }
    .result()

  def _json[A: Reads](f: A => Route): Route                 = (handleRejections(jsonRejectionHandler) & entity(as[A])).apply(f)
  def json[A: Reads](f: A => ToResponseMarshallable): Route = _json[A](a => complete(f(a)))

  val jsonExceptionHandler: ExceptionHandler = ExceptionHandler {
    case JsResultException(err)                                         => complete(WrongJson(errors = err))
    case e: NoSuchElementException                                      => complete(WrongJson(Some(e)))
    case e: ValidationError                                             => complete(ApiError.fromValidationError(e))
    case e: IllegalArgumentException                                    => complete(ApiError.fromValidationError(GenericError(e)))
    case e: AssertionError                                              => complete(ApiError.fromValidationError(GenericError(e)))
    case e: ExecutionException if e.getCause != null && e.getCause != e => jsonExceptionHandler(e.getCause)
  }

  def withAuth: Directive0 = apiKeyHash.fold[Directive0](complete(ApiKeyNotValid)) { hashFromSettings =>
    optionalHeaderValueByType[api_key](()).flatMap {
      case Some(k) if crypto.secureHash(k.value.getBytes()).sameElements(hashFromSettings) => pass
      case _ =>
        optionalHeaderValueByType[deprecated_api_key](()).flatMap {
          case Some(k) if crypto.secureHash(k.value.getBytes()).sameElements(hashFromSettings) => pass
          case _                                                                               => complete(ApiKeyNotValid)
        }
    }
  }

  def processRequest[A: Reads](pathMatcher: String, f: A => ToResponseMarshallable): Route =
    (path(pathMatcher) & post & withAuth) {
      json[A](f)
    }
}

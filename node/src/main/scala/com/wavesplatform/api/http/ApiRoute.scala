package com.wavesplatform.api.http

import java.util.NoSuchElementException
import java.util.concurrent.ExecutionException

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server._
import com.wavesplatform.api.http.ApiError.{ApiKeyNotValid, WrongJson}
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto
import com.wavesplatform.http.{ApiMarshallers, PlayJsonException, `X-Api-Key`, api_key}
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.transaction.TxValidationError.GenericError
import monix.execution.Scheduler
import play.api.libs.json.{JsResultException, Reads}

trait ApiRoute extends Directives with CommonApiFunctions with ApiMarshallers {
  val route: Route

  protected def apiKeyHash: Option[Array[Byte]]

  private val jsonRejectionHandler = RejectionHandler
    .newBuilder()
    .handle { case ValidationRejection(_, Some(PlayJsonException(cause, errors))) => complete(WrongJson(cause, errors)) }
    .result()

  def jsonEntity[A: Reads]: Directive1[A]                   = handleRejections(jsonRejectionHandler) & entity(as[A])
  def json[A: Reads](f: A => ToResponseMarshallable): Route = jsonEntity.apply(a => complete(f(a)))

  val jsonExceptionHandler: ExceptionHandler = ExceptionHandler {
    case JsResultException(err)                                         => complete(WrongJson(errors = err))
    case PlayJsonException(cause, errors)                               => complete(WrongJson(cause, errors))
    case e: NoSuchElementException                                      => complete(WrongJson(Some(e)))
    case e: IllegalArgumentException                                    => complete(ApiError.fromValidationError(GenericError(e)))
    case e: AssertionError                                              => complete(ApiError.fromValidationError(GenericError(e)))
    case e: ExecutionException if e.getCause != null && e.getCause != e => jsonExceptionHandler(e.getCause)
  }

  def withAuth: Directive0 = apiKeyHash.fold[Directive0](complete(ApiKeyNotValid)) { hashFromSettings =>
    optionalHeaderValueByType[`X-Api-Key`](()).flatMap {
      case Some(k) if java.util.Arrays.equals(crypto.secureHash(k.value.getBytes("UTF-8")), hashFromSettings) => pass
      case _ =>
        optionalHeaderValueByType[api_key](()).flatMap {
          case Some(k) if java.util.Arrays.equals(crypto.secureHash(k.value.getBytes("UTF-8")), hashFromSettings) => pass
          case _                                                                                           => complete(ApiKeyNotValid)
        }
    }
  }

  def processRequest[A: Reads](pathMatcher: String, f: A => ToResponseMarshallable): Route =
    (path(pathMatcher) & post & withAuth) {
      json[A](f)
    }

  def extractScheduler: Directive1[Scheduler] = extractExecutionContext.map(ec => Scheduler(ec))
}

trait WithSettings { this: ApiRoute =>
  def settings: RestAPISettings
  protected override lazy val apiKeyHash: Option[Array[Byte]] = Base58.tryDecode(settings.apiKeyHash).toOption
}

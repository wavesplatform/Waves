package com.wavesplatform.api.http

import akka.http.scaladsl.server.*
import cats.data.{Validated, ValidatedNel}
import cats.instances.vector.*
import cats.syntax.traverse.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.utils.{EthEncoding, ScorexLogging}
import monix.execution.{Scheduler, UncaughtExceptionReporter}
import play.api.libs.json.JsObject

import scala.concurrent.duration.DurationInt
import scala.util.Try

trait CustomDirectives extends Directives with ApiMarshallers with ScorexLogging {
  def anyParam(paramName: String, nonEmpty: Boolean = false, limit: Int = Int.MaxValue): Directive1[Iterable[String]] = {
    val baseDirective = (get & pathEndOrSingleSlash & parameter(paramName.as[String].*).map(_.toSeq.reverse)) |
      strictEntity & post & (formField(paramName.as[String].*) |
        entity(as[JsObject]).map { jso =>
          (jso \ s"${paramName}s").as[Iterable[String]]
        })

    baseDirective
      .flatMap {
        case list if nonEmpty && list.isEmpty => reject(MissingQueryParamRejection(paramName))
        case list if list.size > limit        => complete(ApiError.TooBigArrayAllocation(limit))
        case list                             => provide(list)
      }
  }

  def strictEntity: Directive0 = toStrictEntity(5 seconds)

  implicit class SeqDirectiveValidationExt[Source](dir: Directive1[Iterable[Source]]) {
    def massValidate[Result](f: Source => Validated[Source, Result]): Directive1[ValidatedNel[Source, Vector[Result]]] = {
      dir.map(vs => vs.map(f(_).toValidatedNel).toVector.sequence)
    }
  }

  implicit class AnyParamStrDirectiveValidationExt(dir: Directive1[Iterable[String]]) {
    def massValidateIds: Directive1[Vector[ByteStr]] =
      dir.massValidate(str => Validated.fromTry(ByteStr.decodeBase58(str)).leftMap(_ => str)).flatMap {
        case Validated.Valid(a)   => provide(a)
        case Validated.Invalid(e) => complete(ApiError.InvalidIds(e.toList))
      }

    def massValidateEthereumIds: Directive1[Vector[ByteStr]] =
      dir.massValidate(str => Validated.fromTry(Try(ByteStr(EthEncoding.toBytes(str)))).leftMap(_ => str)).flatMap {
        case Validated.Valid(a)   => provide(a)
        case Validated.Invalid(e) => complete(ApiError.InvalidIds(e.toList))
      }
  }

  def extractScheduler: Directive1[Scheduler] =
    extractExecutionContext.map(ec => Scheduler(ec, UncaughtExceptionReporter((t: Throwable) => log.debug("Error processing request", t))))
}

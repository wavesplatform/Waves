package com.wavesplatform.http

import akka.http.scaladsl.marshalling.{Marshaller, PredefinedToEntityMarshallers, ToEntityMarshaller, ToResponseMarshaller}
import akka.http.scaladsl.model.MediaTypes.{`application/json`, `text/plain`}
import akka.http.scaladsl.model.{MessageEntity, StatusCode}
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, PredefinedFromEntityUnmarshallers, Unmarshaller}
import akka.util.ByteString
import com.wavesplatform.api.http.ApiError
import com.wavesplatform.transaction.{Transaction, ValidationError}
import monix.execution.Scheduler
import play.api.libs.json._

import scala.concurrent.Future
import scala.util.control.Exception.nonFatalCatch
import scala.util.control.NoStackTrace

case class PlayJsonException(cause: Option[Throwable] = None, errors: Seq[(JsPath, Seq[JsonValidationError])] = Seq.empty)
    extends IllegalArgumentException
    with NoStackTrace

trait ApiMarshallers {
  import akka.http.scaladsl.marshalling.PredefinedToResponseMarshallers._

  implicit lazy val ApiErrorMarshaller: ToResponseMarshaller[ApiError] =
    fromStatusCodeAndValue[StatusCode, JsValue].compose(ae => (ae.code, ae.json))

  implicit lazy val ValidationErrorMarshaller: ToResponseMarshaller[ValidationError] =
    ApiErrorMarshaller.compose(ve => ApiError.fromValidationError(ve))

  implicit lazy val TransactionJsonWrites: Writes[Transaction] = Writes(_.json())

  private[this] lazy val jsonStringUnmarshaller =
    Unmarshaller.byteStringUnmarshaller
      .forContentTypes(`application/json`)
      .mapWithCharset {
        case (ByteString.empty, _) => throw Unmarshaller.NoContentException
        case (data, charset)       => data.decodeString(charset.nioCharset.name)
      }

  private[this] lazy val jsonStringMarshaller =
    Marshaller.stringMarshaller(`application/json`)

  implicit def playJsonUnmarshaller[A](implicit reads: Reads[A]): FromEntityUnmarshaller[A] = {
    jsonStringUnmarshaller.andThen(Unmarshaller(_ =>
      data =>
        Future {
          val json = nonFatalCatch.withApply(t => throw PlayJsonException(cause = Some(t)))(Json.parse(data))

          json.validate[A] match {
            case JsSuccess(value, _) => value
            case JsError(errors)     => throw PlayJsonException(errors = errors)
          }
        }(ApiMarshallers.executionContext)))
  }

  // preserve support for extracting plain strings from requests
  implicit val stringUnmarshaller: FromEntityUnmarshaller[String] = PredefinedFromEntityUnmarshallers.stringUnmarshaller
  implicit val intUnmarshaller: FromEntityUnmarshaller[Int]       = stringUnmarshaller.map(_.toInt)

  implicit def playJsonMarshaller[A](implicit writes: Writes[A], jsValueToString: JsValue => String = Json.stringify): ToEntityMarshaller[A] = {
    Marshaller
      .futureMarshaller[String, MessageEntity](jsonStringMarshaller)
      .compose((value: A) => Future(jsValueToString(Json.toJson(value)))(ApiMarshallers.executionContext))
  }

  // preserve support for using plain strings as request entities
  implicit val stringMarshaller = PredefinedToEntityMarshallers.stringMarshaller(`text/plain`)
}

object ApiMarshallers extends ApiMarshallers {
  val executionContext = Scheduler.computation(name = "api-marshalling")
}

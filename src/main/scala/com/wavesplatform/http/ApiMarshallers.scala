package com.wavesplatform.http

import scala.util.control.Exception.nonFatalCatch
import scala.util.control.NoStackTrace
import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller, ToResponseMarshaller}
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCode
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, PredefinedFromEntityUnmarshallers, Unmarshaller}
import akka.util.ByteString
import play.api.libs.json._
import scorex.api.http.ApiError
import scorex.transaction.{Transaction, ValidationError}

case class PlayJsonException(
    cause: Option[Throwable] = None,
    errors: Seq[(JsPath, Seq[JsonValidationError])] = Seq.empty) extends IllegalArgumentException with NoStackTrace

trait ApiMarshallers {
  type TRM[A] = ToResponseMarshaller[A]

  import akka.http.scaladsl.marshalling.PredefinedToResponseMarshallers._
  implicit val aem: TRM[ApiError] = fromStatusCodeAndValue[StatusCode, JsValue].compose { ae => ae.code -> ae.json }
  implicit val vem: TRM[ValidationError] = aem.compose(ve => ApiError.fromValidationError(ve))

  implicit val tw: Writes[Transaction] = Writes(_.json)

  private val jsonStringUnmarshaller =
    Unmarshaller.byteStringUnmarshaller
      .forContentTypes(`application/json`)
      .mapWithCharset {
        case (ByteString.empty, _) => throw Unmarshaller.NoContentException
        case (data, charset)       => data.decodeString(charset.nioCharset.name)
      }

  private lazy val jsonStringMarshaller =
    Marshaller.stringMarshaller(`application/json`)

  implicit def playJsonUnmarshaller[A](implicit reads: Reads[A]): FromEntityUnmarshaller[A] =
    jsonStringUnmarshaller map { data =>
      val json = nonFatalCatch.withApply(t => throw PlayJsonException(cause = Some(t)))(Json.parse(data))

      json.validate[A] match {
        case JsSuccess(value, _) => value
        case JsError(errors) => throw PlayJsonException(errors = errors)
      }
    }

  // preserve support for extracting plain strings from requests
  implicit val stringUnmarshaller: FromEntityUnmarshaller[String] = PredefinedFromEntityUnmarshallers.stringUnmarshaller

  implicit def playJsonMarshaller[A](
      implicit writes: Writes[A],
      printer: JsValue => String = Json.prettyPrint): ToEntityMarshaller[A] =
    jsonStringMarshaller.compose(printer).compose(writes.writes)
}

object ApiMarshallers extends ApiMarshallers

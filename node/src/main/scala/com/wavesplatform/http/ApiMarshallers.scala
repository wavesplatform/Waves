package com.wavesplatform.http

import akka.http.scaladsl.common.EntityStreamingSupport
import akka.http.scaladsl.marshalling.{Marshaller, PredefinedToEntityMarshallers, ToEntityMarshaller, ToResponseMarshaller}
import akka.http.scaladsl.model.MediaTypes.{`application/json`, `text/plain`}
import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, PredefinedFromEntityUnmarshallers, Unmarshaller}
import akka.stream.scaladsl.Flow
import akka.util.ByteString
import com.wavesplatform.api.http.ApiError
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.smart.script.trace.{TraceStep, TracedResult}
import play.api.libs.json._

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

  implicit lazy val logWrites: Writes[TraceStep] = Writes(_.json)

  implicit def tracedResultMarshaller[A](implicit writes: Writes[A]): ToResponseMarshaller[TracedResult[ApiError, A]] =
    fromStatusCodeAndValue[StatusCode, JsValue]
      .compose(
        ae =>
          (
            ae.resultE.fold(_.code, _ => StatusCodes.OK),
            ae.resultE.fold(_.json, writes.writes)
          )
      )

  private[this] lazy val jsonStringUnmarshaller =
    Unmarshaller.byteStringUnmarshaller
      .forContentTypes(`application/json`)
      .mapWithCharset {
        case (ByteString.empty, _) => throw Unmarshaller.NoContentException
        case (data, charset)       => data.decodeString(charset.nioCharset.name)
      }

  private[this] lazy val jsonStringMarshaller =
    Marshaller.stringMarshaller(`application/json`)

  private[this] lazy val customJsonStringMarshaller =
    Marshaller.stringMarshaller(CustomJson.jsonWithNumbersAsStrings)

  implicit def playJsonUnmarshaller[A](implicit reads: Reads[A]): FromEntityUnmarshaller[A] =
    jsonStringUnmarshaller.map { data =>
      val json = nonFatalCatch.withApply(t => throw PlayJsonException(cause = Some(t)))(Json.parse(data))

      json.validate[A] match {
        case JsSuccess(value, _) => value
        case JsError(errors)     => throw PlayJsonException(errors = errors)
      }
    }

  implicit val byteStrUnmarshaller: Unmarshaller[String, ByteStr] = Unmarshaller.strict[String, ByteStr] { s =>
    ByteStr.decodeBase58(s).get
  }

  // preserve support for extracting plain strings from requests
  implicit val stringUnmarshaller: FromEntityUnmarshaller[String] = PredefinedFromEntityUnmarshallers.stringUnmarshaller
  implicit val intUnmarshaller: FromEntityUnmarshaller[Int]       = stringUnmarshaller.map(_.toInt)

  implicit def playJsonMarshaller[A](implicit writes: Writes[A], jsValueToString: JsValue => String = Json.stringify): ToEntityMarshaller[A] =
    Marshaller.oneOf(
      jsonStringMarshaller
        .compose(jsValueToString)
        .compose(writes.writes),
      customJsonStringMarshaller
        .compose(CustomJson.writeValueAsString)
        .compose(writes.writes)
    )



  // preserve support for using plain strings as request entities
  implicit val stringMarshaller: ToEntityMarshaller[String] = PredefinedToEntityMarshallers.stringMarshaller(`text/plain`)

  def jsonStream(prefix: String, delimiter: String, suffix: String): EntityStreamingSupport =
    EntityStreamingSupport
      .json()
      .withFramingRenderer(Flow[ByteString].intersperse(ByteString(prefix), ByteString(delimiter), ByteString(suffix)))
}

object ApiMarshallers extends ApiMarshallers

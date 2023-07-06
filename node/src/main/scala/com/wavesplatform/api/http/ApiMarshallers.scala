package com.wavesplatform.api.http

import akka.NotUsed
import akka.http.scaladsl.marshalling.*
import akka.http.scaladsl.model.*
import akka.http.scaladsl.model.MediaTypes.{`application/json`, `text/plain`}
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, PredefinedFromEntityUnmarshallers, Unmarshaller}
import akka.http.scaladsl.util.FastFuture
import akka.stream.scaladsl.{Flow, Source}
import akka.util.ByteString
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import play.api.libs.json.*

import scala.util.control.Exception.nonFatalCatch
import scala.util.control.NoStackTrace

case class PlayJsonException(
    cause: Option[Throwable] = None,
    errors: scala.collection.Seq[(JsPath, scala.collection.Seq[JsonValidationError])] = Seq.empty
) extends IllegalArgumentException
    with NoStackTrace

trait ApiMarshallers extends JsonFormats {
  import akka.http.scaladsl.marshalling.PredefinedToResponseMarshallers.*

  implicit lazy val ApiErrorMarshaller: ToResponseMarshaller[ApiError] =
    fromStatusCodeAndValue[StatusCode, JsValue].compose(ae => (ae.code, ae.json))

  implicit lazy val ValidationErrorMarshaller: ToResponseMarshaller[ValidationError] =
    ApiErrorMarshaller.compose(ve => ApiError.fromValidationError(ve))

  def tracedResultMarshaller[A](includeTrace: Boolean)(implicit writes: OWrites[A]): ToResponseMarshaller[TracedResult[ApiError, A]] =
    fromStatusCodeAndValue[StatusCode, JsValue]
      .compose(
        ae =>
          (
            ae.resultE.fold(_.code, _ => StatusCodes.OK),
            ae.resultE.fold(_.json, writes.writes) ++ (if (includeTrace) Json.obj("trace" -> ae.trace.map(_.loggedJson)) else Json.obj())
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

  private def selectMarshallingForContentType[T](marshallings: Seq[Marshalling[T]], contentType: ContentType): Option[() => T] = {
    contentType match {
      case _: ContentType.Binary | _: ContentType.WithFixedCharset | _: ContentType.WithMissingCharset =>
        marshallings collectFirst { case Marshalling.WithFixedContentType(`contentType`, marshal) => marshal }
      case ContentType.WithCharset(mediaType, charset) =>
        marshallings collectFirst {
          case Marshalling.WithFixedContentType(`contentType`, marshal) => marshal
          case Marshalling.WithOpenCharset(`mediaType`, marshal)        => () => marshal(charset)
        }
    }
  }

  def jsonStreamMarshaller(prefix: String = "[", delimiter: String = ",", suffix: String = "]"): ToResponseMarshaller[Source[JsValue, NotUsed]] = {
    val pjm             = playJsonMarshaller[JsValue].map(_.dataBytes)
    val framingRenderer = Flow[ByteString].intersperse(ByteString(prefix), ByteString(delimiter), ByteString(suffix))
    Marshaller[Source[JsValue, NotUsed], HttpResponse] { implicit ec => source =>
      val availableMarshallingsPerElement = source.mapAsync(1) { t =>
        pjm(t)(ec)
      }
      FastFuture.successful(List(`application/json`, CustomJson.jsonWithNumbersAsStrings).map { contentType =>
        Marshalling.WithFixedContentType(
          contentType,
          () => {
            val bestMarshallingPerElement = availableMarshallingsPerElement map { marshallings =>
              selectMarshallingForContentType(marshallings, contentType)
                .orElse {
                  marshallings collectFirst { case Marshalling.Opaque(marshal) => marshal }
                }
                .getOrElse(throw new NoStrictlyCompatibleElementMarshallingAvailableException[JsValue](contentType, marshallings))
            }

            val marshalledElements: Source[ByteString, NotUsed] =
              bestMarshallingPerElement
                .flatMapConcat(_.apply()) // marshal!
                .via(framingRenderer)

            HttpResponse(entity = HttpEntity(contentType, marshalledElements))
          }
        )
      })
    }
  }
}

object ApiMarshallers extends ApiMarshallers

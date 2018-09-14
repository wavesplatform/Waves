package com.wavesplatform.matcher.api

import akka.http.scaladsl.marshalling.ToResponseMarshaller
import akka.http.scaladsl.model._
import play.api.libs.json.{JsNull, JsValue, Json}

abstract class MatcherResponse(val statusCode: StatusCode, val json: JsValue) {
  def this(code: StatusCode, message: String) =
    this(code,
         Json.obj(
           "success" -> (code == StatusCodes.OK),
           "message" -> message,
           "result"  -> JsNull // For backward compatibility
         ))
}

object MatcherResponse {
  import akka.http.scaladsl.marshalling.PredefinedToResponseMarshallers._
  import com.wavesplatform.http.ApiMarshallers._
  implicit val trm: ToResponseMarshaller[MatcherResponse] =
    fromStatusCodeAndValue[StatusCode, JsValue].compose(mr => mr.statusCode -> mr.json)

  implicit def tuple2MatcherResponse(v: (StatusCode, String)): MatcherResponse = MatcherResponse(v._1, v._2)

  def apply(code: StatusCode, message: String): MatcherResponse = SimpleResponse(code, message)
}

case class SimpleResponse(code: StatusCode, message: String) extends MatcherResponse(code, message)

case class NotImplemented(message: String) extends MatcherResponse(StatusCodes.NotImplemented, message)

case object InvalidSignature extends MatcherResponse(StatusCodes.BadRequest, "Invalid signature")

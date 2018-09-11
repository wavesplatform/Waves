package com.wavesplatform.matcher.api

import akka.http.scaladsl.marshalling.ToResponseMarshaller
import akka.http.scaladsl.model._
import play.api.libs.json.{JsNull, JsValue, Json}

abstract class MatcherResponse(val statusCode: StatusCode, val json: JsValue)

object MatcherResponse {
  import akka.http.scaladsl.marshalling.PredefinedToResponseMarshallers._
  import com.wavesplatform.http.ApiMarshallers._
  implicit val trm: ToResponseMarshaller[MatcherResponse] =
    fromStatusCodeAndValue[StatusCode, JsValue].compose(mr => mr.statusCode -> mr.json)
}

case class StatusCodeMatcherResponse(code: StatusCode, message: String)
    extends MatcherResponse(code,
                            Json.obj(
                              "success" -> (code == StatusCodes.OK),
                              "message" -> message,
                              "result"  -> JsNull // For backward compatibility
                            ))

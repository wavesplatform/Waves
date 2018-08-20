package com.wavesplatform.matcher.api

import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.`Content-Type`
import play.api.libs.json.{JsNull, JsValue, Json}

trait MatcherResponse {
  def toHttpResponse: HttpResponse

  protected def httpJsonResponse(entity: JsValue, status: StatusCode = StatusCodes.OK) = HttpResponse(
    headers = collection.immutable.Seq(`Content-Type`(MediaTypes.`application/json`)),
    entity = Json.stringify(entity)
  )
}

case class StatusCodeMatcherResponse(code: StatusCode, message: String) extends MatcherResponse {
  override def toHttpResponse: HttpResponse = httpJsonResponse(
    entity = Json.obj(
      "success" -> (code == StatusCodes.OK),
      "message" -> message,
      "result"  -> JsNull // For backward compatibility
    ),
    status = code
  )
}

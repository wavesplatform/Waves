package com.wavesplatform.matcher.api

import akka.http.scaladsl.model._
import play.api.libs.json.{JsNull, JsValue, Json}

trait MatcherResponse {
  def toHttpResponse: HttpResponse

  protected def httpJsonResponse(entity: JsValue, status: StatusCode = StatusCodes.OK) = HttpResponse(
    status = status,
    entity = HttpEntity(
      ContentTypes.`application/json`,
      Json.stringify(entity)
    )
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

package com.wavesplatform.matcher.api

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import play.api.libs.json.{JsNull, JsValue, Json}

trait MatcherResponse {
  def json: JsValue
  def code: StatusCode
}

trait GenericMatcherResponse extends MatcherResponse {
  def success: Boolean
  def message: String

  def result: JsValue = JsNull

  def json: JsValue =  Json.obj(
    "success" -> success,
    "message" -> message,
    "result" -> result
  )
  def code: StatusCode = if (success) StatusCodes.OK else StatusCodes.BadRequest
}


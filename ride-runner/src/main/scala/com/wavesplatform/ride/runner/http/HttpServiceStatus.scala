package com.wavesplatform.ride.runner.http

import play.api.libs.json.{JsNull, JsValue, Json, OFormat}

case class HttpServiceStatus(
    healthy: Boolean = false,
    debug: JsValue = JsNull
)

object HttpServiceStatus {
  implicit val httpServiceStatusFormat: OFormat[HttpServiceStatus] = Json.format[HttpServiceStatus]
}

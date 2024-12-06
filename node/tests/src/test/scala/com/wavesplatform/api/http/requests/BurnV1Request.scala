package com.wavesplatform.api.http.requests

import play.api.libs.json.{Json, Reads, Writes}

case class BurnV1Request(sender: String, assetId: String, amount: Long, fee: Long, timestamp: Option[Long] = None)

object BurnV1Request {
  implicit val burnV1Reads: Reads[BurnV1Request] = {
    import play.api.libs.functional.syntax.*
    import play.api.libs.json.*

    ((JsPath \ "sender").read[String] ~
      (JsPath \ "assetId").read[String] ~
      (JsPath \ "amount").read[Long] ~
      (JsPath \ "fee").read[Long] ~
      (JsPath \ "timestamp").readNullable[Long])(BurnV1Request.apply _)
  }

  implicit val burnV1Writes: Writes[BurnV1Request] = Json.writes[BurnV1Request]
}

package com.wavesplatform.api.http.requests

import play.api.libs.json.*

case class BurnV2Request(sender: String, assetId: String, amount: Long, fee: Long, timestamp: Option[Long] = None)

object BurnV2Request {
  implicit val burnV2Reads: Reads[BurnV2Request] = {
    import play.api.libs.functional.syntax.*
    import play.api.libs.json.*

    ((JsPath \ "sender").read[String] ~
      (JsPath \ "assetId").read[String] ~
      (JsPath \ "amount").read[Long] ~
      (JsPath \ "fee").read[Long] ~
      (JsPath \ "timestamp").readNullable[Long])(BurnV2Request.apply _)
  }

  implicit val burnV2Writes: Writes[BurnV2Request] =
    Json.writes[BurnV2Request].transform((request: JsObject) => request + ("version" -> JsNumber(2)))
}

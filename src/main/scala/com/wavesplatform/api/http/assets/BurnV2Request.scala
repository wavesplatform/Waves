package com.wavesplatform.api.http.assets

import play.api.libs.json.{Json, Reads, Writes}

case class BurnV2Request(version: Byte, sender: String, assetId: String, quantity: Long, fee: Long, timestamp: Option[Long] = None)

object BurnV2Request {
  implicit val burnV2Reads: Reads[BurnV2Request] = {
    import play.api.libs.json._
    import play.api.libs.functional.syntax._

    ((JsPath \ 'version).read[Byte] ~
      (JsPath \ 'sender).read[String] ~
      (JsPath \ 'assetId).read[String] ~
      ((JsPath \ 'quantity).read[Long] | (JsPath \ 'amount).read[Long]) ~
      (JsPath \ 'fee).read[Long] ~
      (JsPath \ 'timestamp).readNullable[Long])(BurnV2Request.apply _)
  }

  implicit val burnV2Writes: Writes[BurnV2Request] = Json.writes[BurnV2Request]
}

package com.wavesplatform.api.http.assets
import play.api.libs.json.{Json, Reads, Writes}

case class BurnV1Request(sender: String, assetId: String, quantity: Long, fee: Long, timestamp: Option[Long] = None)

object BurnV1Request {
  implicit val burnV1Reads: Reads[BurnV1Request] = {
    import play.api.libs.json._
    import play.api.libs.functional.syntax._

    ((JsPath \ 'sender).read[String] ~
      (JsPath \ 'assetId).read[String] ~
      ((JsPath \ 'quantity).read[Long] | (JsPath \ 'amount).read[Long]) ~
      (JsPath \ 'fee).read[Long] ~
      (JsPath \ 'timestamp).readNullable[Long])(BurnV1Request.apply _)
  }

  implicit val burnV1Writes: Writes[BurnV1Request] = Json.writes[BurnV1Request]
}

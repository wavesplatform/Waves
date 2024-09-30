package com.wavesplatform.api.http.requests

import play.api.libs.json.*

case class LeaseCancelV2Request(sender: String, txId: String, fee: Long, timestamp: Option[Long] = None)

object LeaseCancelV2Request {
  implicit val leaseCancelRequestReads: Reads[LeaseCancelV2Request] = {
    import play.api.libs.functional.syntax.*
    import play.api.libs.json.*

    ((JsPath \ "sender").read[String] ~
      ((JsPath \ "txId").read[String] | (JsPath \ "leaseId").read[String]) ~
      (JsPath \ "fee").read[Long] ~
      (JsPath \ "timestamp").readNullable[Long])(LeaseCancelV2Request.apply _)
  }

  implicit val leaseCancelRequestWrites: Writes[LeaseCancelV2Request] =
    Json.writes[LeaseCancelV2Request].transform((request: JsObject) => request + ("version" -> JsNumber(2)))
}

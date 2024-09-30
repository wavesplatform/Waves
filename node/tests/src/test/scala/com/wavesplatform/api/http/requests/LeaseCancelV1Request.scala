package com.wavesplatform.api.http.requests

import play.api.libs.json.{Json, Reads, Writes}

case class LeaseCancelV1Request(sender: String, txId: String, fee: Long, timestamp: Option[Long] = None)

object LeaseCancelV1Request {
  implicit val leaseCancelRequestReads: Reads[LeaseCancelV1Request] = {
    import play.api.libs.functional.syntax.*
    import play.api.libs.json.*

    ((JsPath \ "sender").read[String] ~
      ((JsPath \ "txId").read[String] | (JsPath \ "leaseId").read[String]) ~
      (JsPath \ "fee").read[Long] ~
      (JsPath \ "timestamp").readNullable[Long])(LeaseCancelV1Request.apply _)
  }

  implicit val leaseCancelRequestWrites: Writes[LeaseCancelV1Request] = Json.writes[LeaseCancelV1Request]
}

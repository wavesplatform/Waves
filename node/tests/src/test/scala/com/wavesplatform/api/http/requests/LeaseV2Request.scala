package com.wavesplatform.api.http.requests

import play.api.libs.json.{Format, Json}

case class LeaseV2Request(sender: String, amount: Long, fee: Long, recipient: String, timestamp: Option[Long] = None)

object LeaseV2Request {
  implicit val leaseCancelRequestFormat: Format[LeaseV2Request] = Json.format
}

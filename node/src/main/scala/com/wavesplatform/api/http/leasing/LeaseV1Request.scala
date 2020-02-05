package com.wavesplatform.api.http.leasing

import play.api.libs.json.{Format, Json}

case class LeaseV1Request(sender: String,
                          amount: Long,
                          fee: Long,
                          recipient: String,
                          timestamp: Option[Long] = None)

object LeaseV1Request {
  implicit val leaseCancelRequestFormat: Format[LeaseV1Request] = Json.format
}

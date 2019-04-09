package com.wavesplatform.api.http.assets

import play.api.libs.json.{Format, Json}

case class ReissueV1Request(sender: String, assetId: String, quantity: Long, reissuable: Boolean, fee: Long, timestamp: Option[Long] = None)

object ReissueV1Request {
  implicit val reissueFormat: Format[ReissueV1Request] = Json.format
}

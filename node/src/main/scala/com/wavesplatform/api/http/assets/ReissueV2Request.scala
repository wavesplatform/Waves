package com.wavesplatform.api.http.assets

import play.api.libs.json.{Format, Json}

case class ReissueV2Request(sender: String, assetId: String, quantity: Long, reissuable: Boolean, fee: Long, timestamp: Option[Long] = None)

object ReissueV2Request {
  implicit val reissueFormat: Format[ReissueV2Request] = Json.format
}

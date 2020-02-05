package com.wavesplatform.api.http.assets

import play.api.libs.json.{Format, Json}

case class TransferV2Request(
    assetId: Option[String],
    amount: Long,
    feeAssetId: Option[String],
    fee: Long,
    sender: String,
    attachment: Option[String],
    recipient: String,
    timestamp: Option[Long] = None
)

object TransferV2Request {
  implicit val format: Format[TransferV2Request] = Json.format
}

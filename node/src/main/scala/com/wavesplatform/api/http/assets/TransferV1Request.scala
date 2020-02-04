package com.wavesplatform.api.http.assets

import play.api.libs.json.{Format, Json}

case class TransferV1Request(
    assetId: Option[String],
    feeAssetId: Option[String],
    amount: Long,
    fee: Long,
    sender: String,
    attachment: Option[String],
    recipient: String,
    timestamp: Option[Long] = None
)

object TransferV1Request {
  implicit val format: Format[TransferV1Request] = Json.format
}

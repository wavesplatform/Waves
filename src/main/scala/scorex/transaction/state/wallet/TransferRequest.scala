package scorex.transaction.state.wallet

import play.api.libs.json.{Format, Json}

case class TransferRequest(assetId: Option[String],
                           feeAssetId: Option[String],
                           amount: Long,
                           fee: Long,
                           sender: String,
                           attachment: Option[String],
                           recipient: String)

object TransferRequest {
  implicit val transferFormat: Format[TransferRequest] = Json.format
}

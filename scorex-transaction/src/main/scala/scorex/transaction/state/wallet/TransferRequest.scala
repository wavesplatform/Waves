package scorex.transaction.state.wallet

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Reads, Writes}

case class TransferRequest(assetId: Option[String],
                           feeAssetId: Option[String],
                           amount: Long,
                           fee: Long,
                           sender: String,
                           attachment: String,
                           recipient: String)

object TransferRequest {
  implicit val transferWrites: Writes[TransferRequest] = (
    (JsPath \ "assetId").writeNullable[String] and
      (JsPath \ "feeAssetId").writeNullable[String] and
      (JsPath \ "amount").write[Long] and
      (JsPath \ "fee").write[Long] and
      (JsPath \ "sender").write[String] and
      (JsPath \ "attachment").write[String] and
      (JsPath \ "recipient").write[String]
    ) (unlift(TransferRequest.unapply))

  implicit val transferReads: Reads[TransferRequest] = (
    (JsPath \ "assetId").readNullable[String] and
      (JsPath \ "feeAssetId").readNullable[String] and
      (JsPath \ "amount").read[Long] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "sender").read[String] and
      (JsPath \ "attachment").read[String] and
      (JsPath \ "recipient").read[String]
    ) (TransferRequest.apply _)

}

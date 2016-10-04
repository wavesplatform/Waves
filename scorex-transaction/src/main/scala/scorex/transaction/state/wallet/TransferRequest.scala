package scorex.transaction.state.wallet

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Reads, Writes}

case class TransferRequest(assetIdOpt: Option[String],
                           feeAsset: Option[String],
                           amount: Long,
                           feeAmount: Long,
                           sender: String,
                           attachment: String,
                           recipient: String)

object TransferRequest {
  implicit val paymentWrites: Writes[TransferRequest] = (
    (JsPath \ "assetIdOpt").writeNullable[String] and
      (JsPath \ "feeAsset").writeNullable[String] and
      (JsPath \ "amount").write[Long] and
      (JsPath \ "feeAmount").write[Long] and
      (JsPath \ "sender").write[String] and
      (JsPath \ "attachment").write[String] and
      (JsPath \ "recipient").write[String]
    ) (unlift(TransferRequest.unapply))

  implicit val paymentReads: Reads[TransferRequest] = (
    (JsPath \ "assetIdOpt").readNullable[String] and
      (JsPath \ "feeAsset").readNullable[String] and
      (JsPath \ "amount").read[Long] and
      (JsPath \ "feeAmount").read[Long] and
      (JsPath \ "sender").read[String] and
      (JsPath \ "attachment").read[String] and
      (JsPath \ "recipient").read[String]
    ) (TransferRequest.apply _)

}

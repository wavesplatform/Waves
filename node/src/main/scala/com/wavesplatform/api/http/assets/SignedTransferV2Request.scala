package com.wavesplatform.api.http.assets

import cats.implicits._
import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.api.http.BroadcastRequest
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.Proofs
import play.api.libs.functional.syntax._
import play.api.libs.json._

object SignedTransferV2Request {

  implicit val writes: Writes[SignedTransferV2Request] =
    Json.writes[SignedTransferV2Request].transform((request: JsObject) => request + ("version" -> JsNumber(2)))

  implicit val reads: Reads[SignedTransferV2Request] = (
    (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "assetId").readNullable[String] and
      (JsPath \ "recipient").read[String] and
      (JsPath \ "amount").read[Long] and
      (JsPath \ "feeAssetId").readNullable[String] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "attachment").readNullable[String] and
      (JsPath \ "proofs").read[List[ProofStr]]
  )(SignedTransferV2Request.apply _)
}

case class SignedTransferV2Request(senderPublicKey: String,
                                   assetId: Option[String],
                                   recipient: String,
                                   amount: Long,
                                   feeAssetId: Option[String],
                                   fee: Long,
                                   timestamp: Long,
                                   attachment: Option[String],
                                   proofs: List[String])
    extends BroadcastRequest {
  def toTx: Either[ValidationError, TransferTransactionV2] =
    for {
      _sender     <- PublicKey.fromBase58String(senderPublicKey)
      _assetId    <- parseBase58ToAssetId(assetId.filter(_.length > 0), "invalid.assetId")
      _feeAssetId <- parseBase58ToAssetId(feeAssetId.filter(_.length > 0), "invalid.feeAssetId")
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      _recipient  <- AddressOrAlias.fromString(recipient)
      _attachment <- parseBase58(attachment.filter(_.length > 0), "invalid.attachment", TransferTransaction.MaxAttachmentStringSize)
      t           <- TransferTransactionV2.create(_assetId, _sender, _recipient, amount, timestamp, _feeAssetId, fee, _attachment.arr, _proofs)
    } yield t
}

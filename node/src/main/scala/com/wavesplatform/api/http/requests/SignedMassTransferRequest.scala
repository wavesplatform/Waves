package com.wavesplatform.api.http.requests

import com.wavesplatform.account.PublicKey
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import com.wavesplatform.transaction.transfer._
import play.api.libs.functional.syntax._
import play.api.libs.json._

object SignedMassTransferRequest {
  implicit val MassTransferRequestReads: Reads[SignedMassTransferRequest] = (
    (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "assetId").readNullable[String] and
      (JsPath \ "transfers").read[List[Transfer]] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "attachment").readNullable[String] and
      (JsPath \ "proofs").read[Proofs]
  )(SignedMassTransferRequest.apply _)
}

case class SignedMassTransferRequest(
    senderPublicKey: String,
    assetId: Option[String],
    transfers: List[Transfer],
    fee: Long,
    timestamp: Long,
    attachment: Option[String],
    proofs: Proofs
) {
  def toTx: Either[ValidationError, MassTransferTransaction] =
    for {
      _sender     <- PublicKey.fromBase58String(senderPublicKey)
      _assetId    <- parseBase58ToAsset(assetId.filter(_.length > 0), "invalid.assetId")
      _attachment <- parseBase58(attachment.filter(_.length > 0), "invalid.attachment", TransferTransaction.MaxAttachmentStringSize)
      _transfers  <- MassTransferTransaction.parseTransfersList(transfers)
      t           <- MassTransferTransaction.create(1.toByte, _sender, _assetId, _transfers, fee, timestamp, _attachment.arr, proofs)
    } yield t
}

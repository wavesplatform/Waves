package com.wavesplatform.api.http.assets

import cats.implicits._
import com.wavesplatform.account.PublicKey
import com.wavesplatform.api.http.BroadcastRequest
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.Proofs
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
      (JsPath \ "proofs").read[List[ProofStr]]
  )(SignedMassTransferRequest.apply _)
}

case class SignedMassTransferRequest(senderPublicKey: String,
                                     assetId: Option[String],
                                     transfers: List[Transfer],
                                     fee: Long,
                                     timestamp: Long,
                                     attachment: Option[String],
                                     proofs: List[String])
    extends BroadcastRequest {
  def toTx: Either[ValidationError, MassTransferTransaction] =
    for {
      _sender     <- PublicKey.fromBase58String(senderPublicKey)
      _assetId    <- parseBase58ToAssetId(assetId.filter(_.length > 0), "invalid.assetId")
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      _attachment <- parseBase58(attachment.filter(_.length > 0), "invalid.attachment", TransferTransaction.MaxAttachmentStringSize)
      _transfers  <- MassTransferTransaction.parseTransfersList(transfers)
      t           <- MassTransferTransaction.create(_assetId, _sender, _transfers, timestamp, fee, _attachment.arr, _proofs)
    } yield t
}

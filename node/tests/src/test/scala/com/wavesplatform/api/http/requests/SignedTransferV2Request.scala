package com.wavesplatform.api.http.requests

import cats.instances.list._
import cats.syntax.traverse._
import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.transfer._
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

case class SignedTransferV2Request(
    senderPublicKey: String,
    assetId: Option[String],
    recipient: String,
    amount: Long,
    feeAssetId: Option[String],
    fee: Long,
    timestamp: Long,
    attachment: Option[String],
    proofs: List[String]
) {
  def toTx: Either[ValidationError, TransferTransaction] =
    for {
      _sender     <- PublicKey.fromBase58String(senderPublicKey)
      _assetId    <- parseBase58ToAsset(assetId.filter(_.nonEmpty), "invalid.assetId")
      _feeAssetId <- parseBase58ToAsset(feeAssetId.filter(_.nonEmpty), "invalid.feeAssetId")
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      _recipient  <- AddressOrAlias.fromString(recipient)
      _attachment <- parseBase58(attachment.filter(_.nonEmpty), "invalid.attachment", TransferTransaction.MaxAttachmentStringSize)
      tx          <- TransferTransaction.create(2.toByte, _sender, _recipient, _assetId, amount, _feeAssetId, fee, _attachment, timestamp, _proofs)
    } yield tx
}

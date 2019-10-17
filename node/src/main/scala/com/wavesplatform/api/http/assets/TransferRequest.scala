package com.wavesplatform.api.http.assets

import cats.instances.either._
import cats.instances.list._
import cats.syntax.traverse._
import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.api.http.BroadcastRequest
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.transfer.TransferTransaction
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class TransferRequest(
    version: Byte,
    assetId: Option[String],
    feeAssetId: Option[String],
    amount: Long,
    fee: Long,
    attachment: Option[String],
    recipient: String,
    timestamp: Option[Long],
    sender: Option[String],
    senderPublicKey: Option[String],
    proofs: List[String]
) extends BroadcastRequest {
  def toTx: Either[ValidationError, TransferTransaction] =
    for {
      _           <- TransferRequest.validSignedRequest(this)
      _sender     <- PublicKey.fromBase58String(senderPublicKey.get)
      _assetId    <- parseBase58ToAssetId(assetId.filter(_.length > 0), "invalid.assetId")
      _feeAssetId <- parseBase58ToAssetId(feeAssetId.filter(_.length > 0), "invalid.feeAssetId")
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      _recipient  <- AddressOrAlias.fromString(recipient)
      _attachment <- parseBase58(attachment.filter(_.length > 0), "invalid.attachment", TransferTransaction.MaxAttachmentStringSize)
      tx          <- TransferTransaction(version, _assetId, _sender, _recipient, amount, timestamp.get, _feeAssetId, fee, _attachment.arr, _proofs)
    } yield tx
}

object TransferRequest {
  implicit val reads: Reads[TransferRequest] = (
      (JsPath \ "version").readNullable[Byte] and
      (JsPath \ "assetId").readNullable[String] and
      (JsPath \ "feeAssetId").readNullable[String] and
      (JsPath \ "amount").read[Long] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "attachment").readNullable[String] and
      (JsPath \ "recipient").read[String] and
      (JsPath \ "timestamp").readNullable[Long] and
      (JsPath \ "sender").readNullable[String] and
      (JsPath \ "senderPublicKey").readNullable[String] and
      (JsPath \ "signature").readNullable[ProofStr] and
      (JsPath \ "proofs").readNullable[List[ProofStr]]
  ) { (mayBeVersion, assetId, feeAssetId, amount, fee, attachment, recipient, timestamp, sender, senderPublicKey, mayBeSignature, mayBeProofs) =>
    val version = mayBeVersion.getOrElse(1.toByte)
    val proofs  = if (version == 1.toByte) mayBeSignature.fold(List.empty[ProofStr])(List(_)) else mayBeProofs.getOrElse(List.empty)
    TransferRequest(
      version,
      assetId,
      feeAssetId,
      amount,
      fee,
      attachment,
      recipient,
      timestamp,
      sender,
      senderPublicKey,
      proofs
    )
  }

  def validRequest(request: TransferRequest): Either[ValidationError, Unit] =
    Either.cond(request.sender.isDefined, (), GenericError("Invalid sender"))

  def validSignedRequest(request: TransferRequest): Either[ValidationError, Unit] =
    for {
      _ <- Either.cond(request.senderPublicKey.isDefined, (), GenericError("Invalid sender public key"))
      _ <- Either.cond(request.timestamp.isDefined, (), GenericError("Invalid timestamp"))
    } yield ()
}

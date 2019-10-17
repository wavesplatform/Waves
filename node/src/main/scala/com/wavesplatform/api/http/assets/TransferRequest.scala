package com.wavesplatform.api.http.assets

import cats.instances.either._
import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.api.http.{ApiError, BroadcastRequest}
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.TransactionParsers.SignatureStringLength
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Asset, Proofs}
import play.api.libs.json._

case class TransferRequest(
    assetId: Asset,
    feeAssetId: Asset,
    amount: Long,
    fee: Long,
    recipient: AddressOrAlias,
    timestamp: Option[Long],
    sender: Option[String], // address
    senderPublicKey: Option[String],
    version: Byte = 1.toByte,
    attachment: Array[Byte] = Array.emptyByteArray,
    signature: Proofs = Proofs.empty,
    proofs: Proofs = Proofs.empty
) extends BroadcastRequest {

  def versionedProofs: Proofs =
    if (version.toByte == 2.toByte) proofs else signature

  def toTx: Either[ValidationError, TransferTransaction] =
    for {
      sender <- PublicKey.fromBase58String(senderPublicKey.get)
    } yield toTx(sender)

  def toTx(sender: PublicKey): TransferTransaction =
    TransferTransaction(
      version,
      timestamp.getOrElse(0L),
      sender,
      recipient,
      assetId,
      amount,
      feeAssetId,
      fee,
      attachment,
      versionedProofs
    )
}

object TransferRequest extends BroadcastRequest {
  import cats.instances.list._
  import cats.syntax.traverse._

  implicit val addressOrAliasReads: Reads[AddressOrAlias] = Reads {
    case JsString(str) => AddressOrAlias.fromString(str).result
    case _             => JsError("Expected string")
  }

  implicit val attachmentReads: Reads[Array[Byte]] = Reads {
    case JsString(str) if str.nonEmpty => JsSuccess(Base58.tryDecodeWithLimit(str).get)
    case _                             => JsSuccess(Array.emptyByteArray)
  }

  implicit val proofsReads: Reads[Proofs] = Reads {
    case JsString(str) => parseBase58(str, "invalid.signature", SignatureStringLength).map(sig => Proofs(Seq(sig))).result
    case proofs: JsArray =>
      for {
        x <- proofs.validate[List[String]]
        y <- x.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize)).result
        z <- Proofs.create(y).result
      } yield z
    case _ => JsSuccess(Proofs.empty)
  }

  implicit val reads: Reads[TransferRequest] = Json.reads

  private implicit class ValidationOps[A](v: Either[ValidationError, A]) {
    def result: JsResult[A] = v.fold(err => JsError(ApiError.fromValidationError(err).message), JsSuccess(_))
  }
}

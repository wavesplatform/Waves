package com.wavesplatform.api.http.assets

import cats.instances.either._
import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.api.http.BroadcastRequest
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.{Asset, Proofs}
import com.wavesplatform.transaction.TransactionParsers.SignatureStringLength
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.transfer.TransferTransaction
import play.api.libs.json._

case class TransferRequest(
    version: Option[Byte],
    assetId: Option[String],
    feeAssetId: Option[String],
    amount: Long,
    fee: Long,
    recipient: String,
    timestamp: Option[Long],
    sender: Option[String],
    senderPublicKey: Option[String],
    attachment: Option[String],
    signature: Option[String],
    proofs: Option[List[String]]
) extends BroadcastRequest {
  import TransferRequest._

  def toTx: Either[ValidationError, TransferTransaction] =
    for {
      sender <- senderPublicKey match {
        case Some(key) => PublicKey.fromBase58String(key)
        case None      => Left(GenericError("invalid.senderPublicKey"))
      }
      tx <- toTx(sender)
    } yield tx

  def toTx(sender: PublicKey): Either[ValidationError, TransferTransaction] =
    for {
      validRecipient  <- AddressOrAlias.fromString(recipient)
      validAssetId    <- toAsset(assetId)
      validFeeAssetId <- toAsset(feeAssetId)
      validAttachment <- toAttachment(attachment)
      validProofs     <- toProofs(version, signature, proofs)
    } yield TransferTransaction(
      version.getOrElse(1.toByte),
      timestamp.getOrElse(0L),
      sender,
      validRecipient,
      validAssetId,
      amount,
      validFeeAssetId,
      fee,
      validAttachment,
      validProofs
    )

  def toValidTx(sender: PublicKey): Either[ValidationError, TransferTransaction] =
    for {
      tx <- toTx(sender)
      _  <- TransferTransaction.validate(tx)
    } yield tx

  def toValidTx: Either[ValidationError, TransferTransaction] =
    for {
      tx <- toTx
      _  <- TransferTransaction.validate(tx)
    } yield tx
}

object TransferRequest extends BroadcastRequest {
  import cats.instances.list._
  import cats.syntax.either._
  import cats.syntax.traverse._

  implicit val format: Format[TransferRequest] = Json.format

  private def toAsset(maybeAsset: Option[String]): Either[ValidationError, Asset] =
    maybeAsset match {
      case Some(v) if v.nonEmpty => ByteStr.decodeBase58(v).toEither.leftMap(e => GenericError(e.getMessage)).map(IssuedAsset)
      case None                  => Waves.asRight
      case _                     => GenericError("requirement failed: empty string").asLeft
    }

  private def toAttachment(maybeAttachment: Option[String]): Either[ValidationError, Array[Byte]] =
    maybeAttachment match {
      case Some(v) if v.nonEmpty => Base58.tryDecodeWithLimit(v).toEither.leftMap(e => GenericError(e.getMessage))
      case _                     => Array.emptyByteArray.asRight
    }

  private def toProofs(version: Option[Byte], maybeSignature: Option[String], maybeProofs: Option[List[String]]): Either[ValidationError, Proofs] =
    version match {
      case Some(v) if v == 2.toByte =>
        maybeProofs match {
          case Some(proofs) =>
            for {
              proofsBytes <- proofs.traverse(s => parseBase58(s, "invalid.proofs", Proofs.MaxProofStringSize))
              result      <- Proofs.create(proofsBytes)
            } yield result
          case None => Proofs.empty.asRight
        }
      case _ =>
        maybeSignature match {
          case Some(str) => parseBase58(str, "invalid.signature", SignatureStringLength).map(sig => Proofs(Seq(sig)))
          case None      => Proofs.empty.asRight
        }
    }
}

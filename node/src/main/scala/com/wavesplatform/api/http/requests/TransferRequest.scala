package com.wavesplatform.api.http.requests

import cats.syntax.either.*
import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.TxValidationError.{GenericError, TooBigArray}
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Asset, Proofs}
import play.api.libs.json.*

case class TransferRequest(
    version: Option[Byte],
    sender: Option[String],
    senderPublicKey: Option[String],
    recipient: String,
    assetId: Option[Asset],
    amount: Long,
    feeAssetId: Option[Asset],
    fee: Long,
    attachment: Option[String] = None,
    timestamp: Option[Long] = None,
    signature: Option[ByteStr] = None,
    proofs: Option[Proofs] = None
) extends TxBroadcastRequest {
  def toTxFrom(sender: PublicKey): Either[ValidationError, TransferTransaction] =
    for {
      validRecipient <- AddressOrAlias.fromString(recipient)
      validProofs    <- toProofs(signature, proofs)
      attachment <- attachment match {
        case None => ByteStr.empty.asRight
        case Some(x) =>
          if (x.length > TransferTransaction.MaxAttachmentStringSize)
            TooBigArray(
              s"Invalid attachment. Length attachment ${x.length} bytes exceeds maximum size ${TransferTransaction.MaxAttachmentSize} bytes."
            ).asLeft
          else ByteStr.decodeBase58(x).toEither.leftMap(e => GenericError(s"Error parsing base58: ${e.getMessage}"))
      }
      tx <- TransferTransaction.create(
        version.getOrElse(1.toByte),
        sender,
        validRecipient,
        assetId.getOrElse(Asset.Waves),
        amount,
        feeAssetId.getOrElse(Asset.Waves),
        fee,
        attachment,
        timestamp.getOrElse(0L),
        validProofs
      )
    } yield tx
}

object TransferRequest {
  implicit val jsonFormat: Format[TransferRequest] = Json.format
}

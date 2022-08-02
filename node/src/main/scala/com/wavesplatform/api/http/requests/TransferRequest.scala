package com.wavesplatform.api.http.requests

import cats.syntax.either.*
import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.TxValidationError.{GenericError, TooBigInSymbols}
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
        case Some(str) =>
          if (str.length > TransferTransaction.MaxAttachmentStringSize)
            TooBigInSymbols(
              s"Invalid attachment. Length ${str.length} symbols exceeds maximum of ${TransferTransaction.MaxAttachmentStringSize} symbols."
            ).asLeft
          else ByteStr.decodeBase58(str).toEither.leftMap(e => GenericError(s"Error parsing base58: ${e.getMessage}"))
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

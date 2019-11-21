package com.wavesplatform.api.http.requests

import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.transfer.{Attachment, TransferTransaction}
import play.api.libs.json._

case class TransferRequest(
    version: Option[Byte],
    sender: Option[String],
    senderPublicKey: Option[String],
    recipient: String,
    assetId: Option[String],
    amount: Long,
    feeAssetId: Option[String],
    fee: Long,
    attachment: Option[String],
    timestamp: Option[Long],
    signature: Option[String],
    proofs: Option[List[String]]
) extends TxBroadcastRequest {
  def toTxFrom(sender: PublicKey): Either[ValidationError, TransferTransaction] =
    for {
      validRecipient  <- AddressOrAlias.fromString(recipient)
      validAssetId    <- toAsset(assetId)
      validFeeAssetId <- toAsset(feeAssetId)
      validAttachment <- toAttachment(attachment)
      validProofs     <- toProofs(version, signature, proofs)
      tx <- TransferTransaction.create(
        version.getOrElse(1.toByte),
        sender,
        validRecipient,
        validAssetId,
        amount,
        validFeeAssetId,
        fee,
        Attachment.fromBytes(validAttachment),
        timestamp.getOrElse(0L),
        validProofs
      )
    } yield tx
}

object TransferRequest {
  implicit val jsonFormat: Format[TransferRequest] = Json.format
}

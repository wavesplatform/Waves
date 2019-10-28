package com.wavesplatform.api.http.requests

import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.lang.ValidationError
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
) extends TxBroadcastRequest[TransferTransaction] {
  def toTxFrom(sender: PublicKey): Either[ValidationError, TransferTransaction] =
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
}

object TransferRequest {
  implicit val jsonFormat: Format[TransferRequest] = Json.format
}

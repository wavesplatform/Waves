package com.wavesplatform.api.http.requests

import com.wavesplatform.account.PublicKey
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.ReissueTransaction
import play.api.libs.json.Json

case class ReissueRequest(
    version: Option[Byte],
    sender: Option[String],
    senderPublicKey: Option[String],
    asset: String,
    quantity: Long,
    reissuable: Boolean,
    fee: Long,
    timestamp: Option[Long],
    signature: Option[String],
    proofs: Option[List[String]]
) extends TxBroadcastRequest {
  def toTxFrom(sender: PublicKey): Either[ValidationError, ReissueTransaction] =
    for {
      validProofs <- toProofs(version, signature, proofs)
      validAsset  <- toAsset(Some(asset))
      tx <- ReissueTransaction.create(
        version.getOrElse(defaultVersion),
        sender,
        validAsset.asInstanceOf[IssuedAsset],
        quantity,
        reissuable,
        fee,
        timestamp.getOrElse(defaultTimestamp),
        validProofs
      )
    } yield tx
}

object ReissueRequest {
  implicit val jsonFormat = Json.format[ReissueRequest]
}

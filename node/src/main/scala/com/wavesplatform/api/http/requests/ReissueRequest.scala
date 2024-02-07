package com.wavesplatform.api.http.requests

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.assets.ReissueTransaction
import play.api.libs.json.{Format, Json}

case class ReissueRequest(
    version: Option[Byte],
    sender: Option[String],
    senderPublicKey: Option[String],
    assetId: IssuedAsset,
    quantity: Long,
    reissuable: Boolean,
    fee: Long,
    timestamp: Option[Long],
    signature: Option[ByteStr],
    proofs: Option[Proofs]
) extends TxBroadcastRequest[ReissueTransaction] {
  def toTxFrom(sender: PublicKey): Either[ValidationError, ReissueTransaction] =
    for {
      validProofs <- toProofs(signature, proofs)
      tx <- ReissueTransaction.create(
        version.getOrElse(defaultVersion),
        sender,
        assetId,
        quantity,
        reissuable,
        fee,
        timestamp.getOrElse(defaultTimestamp),
        validProofs
      )
    } yield tx
}

object ReissueRequest {
  implicit val jsonFormat: Format[ReissueRequest] = Json.format
}

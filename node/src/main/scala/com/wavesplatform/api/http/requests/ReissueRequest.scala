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
    senderPublicKey: String,
    assetId: IssuedAsset,
    quantity: Long,
    reissuable: Boolean,
    fee: Long,
    timestamp: Option[Long],
    signature: Option[ByteStr],
    proofs: Option[Proofs]
) extends TxBroadcastRequest[ReissueTransaction] {
  def toTx: Either[ValidationError, ReissueTransaction] =
    for {
      validProofs <- toProofs(signature, proofs)
      validSender <- PublicKey.fromBase58String(senderPublicKey)
      tx <- ReissueTransaction.create(
        version.getOrElse(defaultVersion),
        validSender,
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
  given Format[ReissueRequest] = Json.format
}

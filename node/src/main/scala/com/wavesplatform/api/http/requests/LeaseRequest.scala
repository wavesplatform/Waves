package com.wavesplatform.api.http.requests

import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.lease.LeaseTransaction
import play.api.libs.json.{Format, Json}

case class LeaseRequest(
    version: Option[Byte],
    senderPublicKey: String,
    recipient: String,
    amount: Long,
    fee: Long,
    timestamp: Option[Long],
    signature: Option[ByteStr],
    proofs: Option[Proofs]
) extends TxBroadcastRequest[LeaseTransaction] {
  def toTx: Either[ValidationError, LeaseTransaction] =
    for {
      validRecipient <- AddressOrAlias.fromString(recipient)
      validProofs    <- toProofs(signature, proofs)
      validSender    <- PublicKey.fromBase58String(senderPublicKey)
      tx <- LeaseTransaction.create(
        version.getOrElse(1.toByte),
        validSender,
        validRecipient,
        amount,
        fee,
        timestamp.getOrElse(0L),
        validProofs
      )
    } yield tx
}

object LeaseRequest {
  given Format[LeaseRequest] = Json.format
}

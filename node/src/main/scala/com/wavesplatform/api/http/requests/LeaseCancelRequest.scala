package com.wavesplatform.api.http.requests

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.lease.LeaseCancelTransaction
import play.api.libs.json.Json

case class LeaseCancelRequest(
    version: Option[Byte],
    sender: Option[String],
    senderPublicKey: Option[String],
    leaseId: String,
    fee: Long,
    timestamp: Option[Long],
    signature: Option[String],
    proofs: Option[List[String]]
) extends TxBroadcastRequest {
  def toTxFrom(sender: PublicKey): Either[ValidationError, LeaseCancelTransaction] =
    for {
      validProofs <- toProofs(version, signature, proofs)
      tx <- LeaseCancelTransaction.create(
        version.getOrElse(1.toByte),
        sender,
        Base58.decode(leaseId),
        fee,
        timestamp.getOrElse(0L),
        validProofs
      )
    } yield tx
}

object LeaseCancelRequest {
  implicit val jsonFormat = Json.format[LeaseCancelRequest]
}

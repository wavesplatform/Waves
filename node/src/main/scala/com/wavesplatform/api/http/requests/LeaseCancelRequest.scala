package com.wavesplatform.api.http.requests

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.lease.LeaseCancelTransaction
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class LeaseCancelRequest(
    version: Option[Byte],
    sender: Option[String],
    senderPublicKey: Option[String],
    leaseId: String,
    fee: Long,
    timestamp: Option[Long],
    signature: Option[ByteStr],
    proofs: Option[Proofs]
) extends TxBroadcastRequest[LeaseCancelTransaction] {
  def toTxFrom(sender: PublicKey): Either[ValidationError, LeaseCancelTransaction] =
    for {
      validProofs  <- toProofs(signature, proofs)
      validLeaseId <- parseBase58(leaseId, "invalid.leaseTx", DigestStringLength)
      tx <- LeaseCancelTransaction.create(
        version.getOrElse(1.toByte),
        sender,
        validLeaseId,
        fee,
        timestamp.getOrElse(0L),
        validProofs
      )
    } yield tx
}

object LeaseCancelRequest {
  import com.wavesplatform.utils.byteStrFormat
  implicit val jsonFormat: Format[LeaseCancelRequest] = Format(
    ((JsPath \ "version").readNullable[Byte] and
      (JsPath \ "sender").readNullable[String] and
      (JsPath \ "senderPublicKey").readNullable[String] and
      (JsPath \ "leaseId").read[String].orElse((JsPath \ "txId").read[String]) and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "timestamp").readNullable[Long] and
      (JsPath \ "signature").readNullable[ByteStr] and
      (JsPath \ "proofs").readNullable[Proofs])(LeaseCancelRequest.apply _),
    Json.writes[LeaseCancelRequest]
  )
}

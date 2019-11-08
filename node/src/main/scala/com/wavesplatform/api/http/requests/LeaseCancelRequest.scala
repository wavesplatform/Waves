package com.wavesplatform.api.http.requests

import com.wavesplatform.account.PublicKey
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.lease.LeaseCancelTransaction
import play.api.libs.json.{Format, Json}

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
      validProofs  <- toProofs(version, signature, proofs)
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
  implicit val jsonFormat = Format(
    {
      import play.api.libs.functional.syntax._
      import play.api.libs.json._
      ((JsPath \ "version").readNullable[Byte] and
        (JsPath \ "sender").readNullable[String] and
        (JsPath \ "senderPublicKey").readNullable[String] and
        (JsPath \ "leaseId").read[String].orElse((JsPath \ "txId").read[String]) and
        (JsPath \ "fee").read[Long] and
        (JsPath \ "timestamp").readNullable[Long] and
        (JsPath \ "signature").readNullable[String] and
        (JsPath \ "proofs").readNullable[List[String]])(LeaseCancelRequest.apply _)
    },
    Json.writes[LeaseCancelRequest]
  )
}

package com.wavesplatform.api.http.requests

import cats.instances.list._
import cats.syntax.traverse._
import com.wavesplatform.account.PublicKey
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.lease.LeaseCancelTransaction
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class SignedLeaseCancelV2Request(
    chainId: Byte,
    senderPublicKey: String,
    leaseId: String,
    timestamp: Long,
    proofs: List[String],
    fee: Long
) {
  def toTx: Either[ValidationError, LeaseCancelTransaction] =
    for {
      _sender     <- PublicKey.fromBase58String(senderPublicKey)
      _leaseTx    <- parseBase58(leaseId, "invalid.leaseTx", SignatureStringLength)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      _t          <- LeaseCancelTransaction.create(2.toByte, _sender, _leaseTx, fee, timestamp, _proofs)
    } yield _t
}
object SignedLeaseCancelV2Request {
  implicit val reads: Reads[SignedLeaseCancelV2Request] = (
    (JsPath \ "chainId").read[Byte] and
      (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "leaseId").read[String] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "proofs").read[List[String]] and
      (JsPath \ "fee").read[Long]
  )(SignedLeaseCancelV2Request.apply _)

  implicit val writes: OWrites[SignedLeaseCancelV2Request] =
    Json.writes[SignedLeaseCancelV2Request].transform((request: JsObject) => request + ("version" -> JsNumber(2)))
}

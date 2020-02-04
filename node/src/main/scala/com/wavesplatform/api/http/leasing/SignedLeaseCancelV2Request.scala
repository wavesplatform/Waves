package com.wavesplatform.api.http.leasing

import cats.implicits._
import com.wavesplatform.account.PublicKey
import com.wavesplatform.api.http.BroadcastRequest
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.TransactionParsers.SignatureStringLength
import com.wavesplatform.transaction.lease.LeaseCancelTransactionV2
import com.wavesplatform.transaction.Proofs
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class SignedLeaseCancelV2Request(chainId: Byte,
                                      senderPublicKey: String,
                                      leaseId: String,
                                      timestamp: Long,
                                      proofs: List[String],
                                      fee: Long)
    extends BroadcastRequest {
  def toTx: Either[ValidationError, LeaseCancelTransactionV2] =
    for {
      _sender     <- PublicKey.fromBase58String(senderPublicKey)
      _leaseTx    <- parseBase58(leaseId, "invalid.leaseTx", SignatureStringLength)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      _t          <- LeaseCancelTransactionV2.create(chainId, _sender, _leaseTx, fee, timestamp, _proofs)
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

  implicit val writes =
    Json.writes[SignedLeaseCancelV2Request].transform((request: JsObject) => request + ("version" -> JsNumber(2)))
}

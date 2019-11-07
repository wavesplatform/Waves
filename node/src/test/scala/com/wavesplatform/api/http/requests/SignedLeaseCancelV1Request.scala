package com.wavesplatform.api.http.requests

import com.wavesplatform.account.PublicKey
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.lease.LeaseCancelTransaction
import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class SignedLeaseCancelV1Request(
    @ApiModelProperty(value = "Base58 encoded sender public key", required = true)
    senderPublicKey: String,
    @ApiModelProperty(value = "Base58 encoded lease transaction id", required = true)
    leaseId: String,
    @ApiModelProperty(required = true)
    timestamp: Long,
    @ApiModelProperty(required = true)
    signature: String,
    @ApiModelProperty(required = true)
    fee: Long
) {
  def toTx: Either[ValidationError, LeaseCancelTransaction] =
    for {
      _sender    <- PublicKey.fromBase58String(senderPublicKey)
      _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
      _leaseTx   <- parseBase58(leaseId, "invalid.leaseTx", SignatureStringLength)
      _t         <- LeaseCancelTransaction.create(1.toByte, _sender, _leaseTx, fee, timestamp, Proofs(_signature))
    } yield _t
}

object SignedLeaseCancelV1Request {
  implicit val reads: Reads[SignedLeaseCancelV1Request] = (
    (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "txId").read[String].orElse((JsPath \ "leaseId").read[String]) and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "signature").read[String] and
      (JsPath \ "fee").read[Long]
  )(SignedLeaseCancelV1Request.apply _)

  implicit val writes: Writes[SignedLeaseCancelV1Request] = Json.writes[SignedLeaseCancelV1Request]
}

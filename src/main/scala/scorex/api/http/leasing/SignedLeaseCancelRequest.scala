package scorex.api.http.leasing

import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json._
import scorex.account.PublicKeyAccount
import scorex.api.http.BroadcastRequest
import scorex.transaction.TransactionParsers.SignatureStringLength
import scorex.transaction.ValidationError
import scorex.transaction.lease.LeaseCancelTransaction

case class SignedLeaseCancelRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                    senderPublicKey: String,
                                    @ApiModelProperty(value = "Base58 encoded lease transaction id", required = true)
                                    txId: String,
                                    @ApiModelProperty(required = true)
                                    timestamp: Long,
                                    @ApiModelProperty(required = true)
                                    signature: String,
                                    @ApiModelProperty(required = true)
                                    fee: Long)
    extends BroadcastRequest {
  def toTx: Either[ValidationError, LeaseCancelTransaction] =
    for {
      _sender    <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
      _leaseTx   <- parseBase58(txId, "invalid.leaseTx", SignatureStringLength)
      _t         <- LeaseCancelTransaction.create(_sender, _leaseTx, fee, timestamp, _signature)
    } yield _t
}

object SignedLeaseCancelRequest {
  implicit val reads: Reads[SignedLeaseCancelRequest] = (
    (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "txId").read[String].orElse((JsPath \ "leaseId").read[String]) and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "signature").read[String] and
      (JsPath \ "fee").read[Long]
  )(SignedLeaseCancelRequest.apply _)

  implicit val writes: Writes[SignedLeaseCancelRequest] = Json.writes[SignedLeaseCancelRequest]
}

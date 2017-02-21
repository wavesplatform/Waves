package scorex.api.http.leasing

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import scorex.account.{Account, PublicKeyAccount}
import scorex.api.http.{BroadcastRequest}
import scorex.crypto.encode.Base58
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.InvalidAddress
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.TypedTransaction.SignatureStringLength

case class SignedLeaseCancelRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                    senderPublicKey: String,
                                    @ApiModelProperty(value = "Base58 encoded lease transaction id", required = true)
                                    txId: String,
                                    @ApiModelProperty(required = true)
                                    timestamp: Long,
                                    @ApiModelProperty(required = true)
                                    signature: String,
                                    @ApiModelProperty(required = true)
                                    fee: Long) extends BroadcastRequest {
  def toTx: Either[ValidationError, LeaseCancelTransaction] = for {
    _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
    _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
    _leaseTx <- parseBase58(txId, "invalid.leaseTx", SignatureStringLength)
    _t <- LeaseCancelTransaction.create(
      _sender,
      _leaseTx,
      fee,
      timestamp,
      _signature)
  } yield _t
}

object SignedLeaseCancelRequest {
  implicit val leaseCancelRequestFormat: Format[SignedLeaseCancelRequest] = Json.format
}


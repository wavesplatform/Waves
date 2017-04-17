package scorex.api.http.leasing

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import scorex.account.{Account, PublicKeyAccount}
import scorex.api.http.BroadcastRequest
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.InvalidAddress
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.TypedTransaction.SignatureStringLength

case class SignedLeaseRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                              senderPublicKey: String,
                              @ApiModelProperty(required = true)
                              amount: Long,
                              @ApiModelProperty(required = true)
                              fee: Long,
                              @ApiModelProperty(value = "Recipient address", required = true)
                              recipient: String,
                              @ApiModelProperty(required = true)
                              timestamp: Long,
                              @ApiModelProperty(required = true)
                              signature: String) extends BroadcastRequest {
  def toTx: Either[ValidationError, LeaseTransaction] = for {
    _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
    _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
    _recipient <- if (Account.isValidAddress(recipient)) Right(new Account(recipient)) else Left(InvalidAddress)
    _t <- LeaseTransaction.create(_sender, amount, fee, timestamp, _recipient, _signature)
  } yield _t
}

object SignedLeaseRequest {
  implicit val broadcastLeaseRequestReadsFormat: Format[SignedLeaseRequest] = Json.format
}

package scorex.api.http.leasing

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import scorex.account.{Account, PublicKeyAccount}
import scorex.crypto.encode.Base58
import scorex.transaction.ValidationError
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.api.http.formats._

case class SignedLeaseRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                              sender: PublicKeyAccount,
                              @ApiModelProperty(required = true)
                              amount: Long,
                              @ApiModelProperty(required = true)
                              fee: Long,
                              @ApiModelProperty(value = "Recipient address", required = true)
                              recipient: Account,
                              @ApiModelProperty(required = true)
                              timestamp: Long,
                              @ApiModelProperty(required = true)
                              signature: String) {
  def toTx: Either[ValidationError, LeaseTransaction] = LeaseTransaction.create(
    sender,
    amount,
    fee,
    timestamp,
    recipient,
    Base58.decode(signature).get
  )
}

object SignedLeaseRequest {
  implicit val broadcastLeaseRequestReadsFormat = Json.format[SignedLeaseRequest]
}

package scorex.api.http.leasing

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import scorex.account.PublicKeyAccount
import scorex.crypto.encode.Base58
import scorex.transaction.ValidationError
import scorex.transaction.lease.LeaseCancelTransaction
import scorex.api.http.formats._

case class SignedLeaseCancelRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                    senderPublicKey: PublicKeyAccount,
                                    @ApiModelProperty(value = "Base58 encoded lease transaction id", required = true)
                                    txId: String,
                                    @ApiModelProperty(required = true)
                                    timestamp: Long,
                                    @ApiModelProperty(required = true)
                                    signature: String,
                                    @ApiModelProperty(required = true)
                                    fee: Long) {
  def toTx: Either[ValidationError, LeaseCancelTransaction] = LeaseCancelTransaction.create(
    senderPublicKey,
    Base58.decode(txId).get,
    fee,
    timestamp,
    Base58.decode(signature).get)
}

object SignedLeaseCancelRequest {
  implicit val leaseCancelRequestFormat = Json.format[SignedLeaseCancelRequest]
}


package scorex.api.http.leasing

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import scorex.account.{Account, PublicKeyAccount}
import scorex.api.http.formats._

case class LeaseRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                        sender: PublicKeyAccount,
                        @ApiModelProperty(required = true)
                        amount: Long,
                        @ApiModelProperty(required = true)
                        fee: Long,
                        @ApiModelProperty(value = "Recipient address", required = true)
                        recipient: Account)

object LeaseRequest {
  implicit val leaseCancelRequestFormat = Json.format[LeaseRequest]
}

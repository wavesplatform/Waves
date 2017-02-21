package scorex.api.http.leasing

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import scorex.account.Account

case class LeaseRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                        sender: String,
                        @ApiModelProperty(required = true)
                        amount: Long,
                        @ApiModelProperty(required = true)
                        fee: Long,
                        @ApiModelProperty(value = "Recipient address", required = true)
                        recipient: String)

object LeaseRequest {
  implicit val leaseCancelRequestFormat: Format[LeaseRequest] = Json.format
}

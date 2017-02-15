package scorex.api.http.leasing

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import scorex.account.PublicKeyAccount
import scorex.api.http.formats._

case class LeaseCancelRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                              sender: PublicKeyAccount,
                              @ApiModelProperty(value = "Base58 encoded lease transaction id", required = true)
                              txId: String,
                              @ApiModelProperty(required = true)
                              fee: Long)

object LeaseCancelRequest {
  implicit val leaseCancelRequestFormat = Json.format[LeaseCancelRequest]
}

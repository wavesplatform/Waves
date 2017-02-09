package scorex.api.http.leasing

import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Reads}

case class LeaseRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                        sender: String,
                        @ApiModelProperty(required = true)
                        amount: Long,
                        @ApiModelProperty(required = true)
                        fee: Long,
                        @ApiModelProperty(value = "Recipient address", required = true)
                        recipient: String)

object LeaseRequest {
  implicit val leaseRequestReads : Reads[LeaseRequest] = (
    (JsPath \ "sender").read[String] and
      (JsPath \ "amount").read[Long] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "recipient").read[String]
    ) (LeaseRequest.apply _)
}

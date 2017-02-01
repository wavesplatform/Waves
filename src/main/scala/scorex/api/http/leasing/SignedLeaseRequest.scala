package scorex.api.http.leasing

import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Reads}
import scorex.api.http.formats.SignatureReads

case class SignedLeaseRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                 sender: String,
                              @ApiModelProperty(required = true)
                                 amount: Long,
                              @ApiModelProperty(required = true)
                                 fee: Long,
                              @ApiModelProperty(required = true)
                                 untilBlock: Long,
                              @ApiModelProperty(value = "Recipient address", required = true)
                                 recipient: String,
                              @ApiModelProperty(required = true)
                                 signature: String)

object SignedLeaseRequest {
  implicit val broadcastLeaseRequestReads: Reads[SignedLeaseRequest] = (
    (JsPath \ "sender").read[String] and
      (JsPath \ "amount").read[Long] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "untilBlock").read[Long] and
      (JsPath \ "recipient").read[String] and
      (JsPath \ "signature").read[String](SignatureReads)
    ) (SignedLeaseRequest.apply _)
}
package scorex.api.http.leasing

import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Reads}
import scorex.api.http.formats.SignatureReads

case class SignedLeaseCancelRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                       sender: String,
                                    @ApiModelProperty(value = "Base58 encoded lease transaction id", required = true)
                                       txId: String,
                                    @ApiModelProperty(required = true)
                                       signature: String)

object SignedLeaseCancelRequest {
  implicit val leaseCancelRequest: Reads[SignedLeaseCancelRequest] = (
    (JsPath \ "sender").read[String] and
      (JsPath \ "txId").read[String] and
      (JsPath \ "signature").read[String](SignatureReads)
    ) (SignedLeaseCancelRequest.apply _)
}


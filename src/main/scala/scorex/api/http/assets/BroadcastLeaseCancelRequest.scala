package scorex.api.http.assets

import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Reads}
import scorex.api.http.formats.SignatureReads

case class BroadcastLeaseCancelRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                       sender: String,
                                       @ApiModelProperty(value = "Base58 encoded lease transaction id", required = true)
                                       txId: String,
                                       @ApiModelProperty(required = true)
                                       signature: String)

object BroadcastLeaseCancelRequest {
  implicit val leaseCancelRequest: Reads[BroadcastLeaseCancelRequest] = (
    (JsPath \ "sender").read[String] and
      (JsPath \ "txId").read[String] and
      (JsPath \ "signature").read[String](SignatureReads)
    ) (BroadcastLeaseCancelRequest.apply _)
}


package scorex.api.http.assets

import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Reads}

case class LeaseCancelRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                              sender: String,
                              @ApiModelProperty(value = "Base58 encoded lease transaction id", required = true)
                              txId: String)

object LeaseCancelRequest {
  implicit val leaseCancelRequest: Reads[LeaseCancelRequest] = (
    (JsPath \ "sender").read[String] and
      (JsPath \ "txId").read[String]
    ) (LeaseCancelRequest.apply _)
}

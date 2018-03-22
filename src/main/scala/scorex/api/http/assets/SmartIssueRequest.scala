package scorex.api.http.assets

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class SmartIssueRequest(@ApiModelProperty(required = true)
                             version: Byte,
                             @ApiModelProperty(required = true)
                             chainId: Byte,
                             @ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
                             sender: String,
                             @ApiModelProperty(value = "Base58 encoded name of Asset", required = true)
                             name: Array[Byte],
                             @ApiModelProperty(value = "Base58 encoded description of Asset", required = true)
                             description: Array[Byte],
                             @ApiModelProperty(required = true, example = "1000000")
                             quantity: Long,
                             @ApiModelProperty(allowableValues = "range[0,8]", example = "8", dataType = "integer", required = true)
                             decimals: Byte,
                             @ApiModelProperty(required = true)
                             reissuable: Boolean,
                             script: Option[String],
                             @ApiModelProperty(required = true)
                             fee: Long,
                             @ApiModelProperty(required = true)
                             timestamp: Long)

object SmartIssueRequest {
  implicit val jsonFormat: Format[SmartIssueRequest] = Json.format
}

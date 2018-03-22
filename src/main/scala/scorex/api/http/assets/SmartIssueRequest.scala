package scorex.api.http.assets

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class SmartIssueRequest(@ApiModelProperty(required = true)
                             version: Byte,
                             @ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
                             sender: String,
                             @ApiModelProperty(value = "Base58 encoded name of Asset", required = true)
                             name: String,
                             @ApiModelProperty(value = "Base58 encoded description of Asset", required = true)
                             description: String,
                             @ApiModelProperty(required = true, example = "1000000")
                             quantity: Long,
                             @ApiModelProperty(allowableValues = "range[0,8]", example = "8", dataType = "integer", required = true)
                             decimals: Byte,
                             @ApiModelProperty(required = true)
                             reissuable: Boolean,
                             script: Option[String],
                             @ApiModelProperty(required = true)
                             fee: Long,
                             timestamp: Option[Long])

object SmartIssueRequest {
  implicit val jsonFormat: Format[SmartIssueRequest] = Json.format
}

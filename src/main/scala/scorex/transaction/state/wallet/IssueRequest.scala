package scorex.transaction.state.wallet

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}


case class IssueRequest(sender: String,
  name: String,
  description: String,
  quantity: Long,
  @ApiModelProperty(allowableValues = "range[0,8]", example = "8", dataType = "integer", required = true)
  decimals: Byte,
  reissuable: Boolean,
  fee: Long)

object IssueRequest {
  implicit val issueFormat: Format[IssueRequest] = Json.format
}

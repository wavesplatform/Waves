package scorex.transaction.state.wallet

import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Reads, Writes}


case class IssueRequest(sender: String,
  name: String,
  description: String,
  quantity: Long,
  @ApiModelProperty(allowableValues = "range[0,8]", example = "8", dataType = "integer", required = true)
  decimals: Byte,
  reissuable: Boolean,
  fee: Long) {
}

object IssueRequest {
  implicit val issueWrites: Writes[IssueRequest] = (
    (JsPath \ "sender").write[String] and
      (JsPath \ "name").write[String] and
      (JsPath \ "description").write[String] and
      (JsPath \ "quantity").write[Long] and
      (JsPath \ "decimals").write[Byte] and
      (JsPath \ "reissuable").write[Boolean] and
      (JsPath \ "fee").write[Long]
    ) (unlift(IssueRequest.unapply))


  implicit val issueReads: Reads[IssueRequest] = (
    (JsPath \ "sender").read[String] and
      (JsPath \ "name").read[String] and
      (JsPath \ "description").read[String] and
      (JsPath \ "quantity").read[Long] and
      (JsPath \ "decimals").read[Byte] and
      (JsPath \ "reissuable").read[Boolean] and
      (JsPath \ "fee").read[Long]
    ) (IssueRequest.apply _)

}
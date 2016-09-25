package scorex.transaction.state.wallet

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Reads, Writes}
import scorex.transaction.assets.IssueTransaction

import scala.util.Try


case class IssueRequest(sender: String,
                        assetIdOpt: Option[String],
                        name: String,
                        description: String,
                        quantity: Long,
                        decimals: Byte,
                        reissuable: Boolean,
                        fee: Long) {
}

object IssueRequest {
  implicit val issueWrites: Writes[IssueRequest] = (
    (JsPath \ "sender").write[String] and
      (JsPath \ "assetIdOpt").writeNullable[String] and
      (JsPath \ "name").write[String] and
      (JsPath \ "description").write[String] and
      (JsPath \ "quantity").write[Long] and
      (JsPath \ "decimals").write[Byte] and
      (JsPath \ "reissuable").write[Boolean] and
      (JsPath \ "fee").write[Long]
    ) (unlift(IssueRequest.unapply))


  implicit val paymentReads: Reads[IssueRequest] = (
    (JsPath \ "sender").read[String] and
      (JsPath \ "assetIdOpt").readNullable[String] and
      (JsPath \ "name").read[String] and
      (JsPath \ "description").read[String] and
      (JsPath \ "quantity").read[Long] and
      (JsPath \ "decimals").read[Byte] and
      (JsPath \ "reissuable").read[Boolean] and
      (JsPath \ "fee").read[Long]
    ) (IssueRequest.apply _)

}
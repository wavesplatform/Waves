package scorex.transaction.state.wallet

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Reads, Writes}
import scorex.transaction.assets.IssueTransaction

import scala.util.Try


case class ReissueRequest(sender: String,
                        assetId: String,
                        quantity: Long,
                        reissuable: Boolean,
                        fee: Long)

object ReissueRequest {
  implicit val reissueWrites: Writes[ReissueRequest] = (
    (JsPath \ "sender").write[String] and
    (JsPath \ "assetId").write[String] and
      (JsPath \ "quantity").write[Long] and
      (JsPath \ "reissuable").write[Boolean] and
      (JsPath \ "fee").write[Long]
    ) (unlift(ReissueRequest.unapply))


  implicit val reissueReads: Reads[ReissueRequest] = (
    (JsPath \ "sender").read[String] and
      (JsPath \ "assetId").read[String] and
      (JsPath \ "quantity").read[Long] and
      (JsPath \ "reissuable").read[Boolean] and
      (JsPath \ "fee").read[Long]
    ) (ReissueRequest.apply _)

}
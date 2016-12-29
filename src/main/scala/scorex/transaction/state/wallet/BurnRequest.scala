package scorex.transaction.state.wallet

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Reads, Writes}


case class BurnRequest(sender: String,
                       assetId: String,
                       quantity: Long,
                       fee: Long)

object BurnRequest {
  implicit val reissueWrites: Writes[BurnRequest] = (
    (JsPath \ "sender").write[String] and
      (JsPath \ "assetId").write[String] and
      (JsPath \ "quantity").write[Long] and
      (JsPath \ "fee").write[Long]
    ) (unlift(BurnRequest.unapply))


  implicit val reissueReads: Reads[BurnRequest] = (
    (JsPath \ "sender").read[String] and
      (JsPath \ "assetId").read[String] and
      (JsPath \ "quantity").read[Long] and
      (JsPath \ "fee").read[Long]
    ) (BurnRequest.apply _)

}
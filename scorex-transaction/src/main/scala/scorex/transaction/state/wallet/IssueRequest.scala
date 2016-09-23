package scorex.transaction.state.wallet
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Reads, Writes}


case class IssueRequest(amount: Long, fee: Long, sender: String, recipient: String)

object IssueRequest {
  implicit val paymentWrites: Writes[IssueRequest] = (
    (JsPath \ "amount").write[Long] and
      (JsPath \ "fee").write[Long] and
      (JsPath \ "sender").write[String] and
      (JsPath \ "recipient").write[String]
    ) (unlift(IssueRequest.unapply))

  implicit val paymentReads: Reads[IssueRequest] = (
    (JsPath \ "amount").read[Long] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "sender").read[String] and
      (JsPath \ "recipient").read[String]
    ) (IssueRequest.apply _)

}
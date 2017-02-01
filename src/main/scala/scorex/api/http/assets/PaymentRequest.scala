package scorex.api.http.assets

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Reads, Writes}

case class PaymentRequest(amount: Long, fee: Long, sender: String, recipient: String)

object PaymentRequest {
  implicit val paymentWrites: Writes[PaymentRequest] = (
    (JsPath \ "amount").write[Long] and
      (JsPath \ "fee").write[Long] and
      (JsPath \ "sender").write[String] and
      (JsPath \ "recipient").write[String]
    ) (unlift(PaymentRequest.unapply))

  implicit val paymentReads: Reads[PaymentRequest] = (
    (JsPath \ "amount").read[Long] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "sender").read[String] and
      (JsPath \ "recipient").read[String]
    ) (PaymentRequest.apply _)

}

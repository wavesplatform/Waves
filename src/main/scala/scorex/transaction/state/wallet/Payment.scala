package scorex.transaction.state.wallet

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Reads, Writes}

case class Payment(amount: Long, fee: Long, sender: String, recipient: String)

object Payment {
  implicit val paymentWrites: Writes[Payment] = (
    (JsPath \ "amount").write[Long] and
      (JsPath \ "fee").write[Long] and
      (JsPath \ "sender").write[String] and
      (JsPath \ "recipient").write[String]
    ) (unlift(Payment.unapply))

  implicit val paymentReads: Reads[Payment] = (
    (JsPath \ "amount").read[Long] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "sender").read[String] and
      (JsPath \ "recipient").read[String]
    ) (Payment.apply _)

}

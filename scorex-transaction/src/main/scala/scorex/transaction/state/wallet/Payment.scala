package scorex.transaction.state.wallet

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Reads, Writes}

case class Payment(amount: Double, fee: Double, sender: String, recipient: String)

object Payment {
  implicit val paymentWrites: Writes[Payment] = (
    (JsPath \ "amount").write[Double] and
      (JsPath \ "fee").write[Double] and
      (JsPath \ "sender").write[String] and
      (JsPath \ "recipient").write[String]
    ) (unlift(Payment.unapply))

  implicit val locationReads: Reads[Payment] = (
    (JsPath \ "amount").read[Double] and
      (JsPath \ "fee").read[Double] and
      (JsPath \ "sender").read[String] and
      (JsPath \ "recipient").read[String]
    ) (Payment.apply _)

}

package scorex.waves.transaction

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Reads, Writes}

case class SignedPayment(timestamp: Long,
                         amount: Long,
                         fee: Long,
                         recipient: String,
                         senderPublicKey: String,
                         sender: String,
                         signature: String)

object SignedPayment {
  implicit val paymentWrites: Writes[SignedPayment] = (
    (JsPath \ "timestamp").write[Long] and
      (JsPath \ "amount").write[Long] and
      (JsPath \ "fee").write[Long] and
      (JsPath \ "recipient").write[String] and
      (JsPath \ "senderPublicKey").write[String] and
      (JsPath \ "sender").write[String] and
      (JsPath \ "signature").write[String]
    ) (unlift(SignedPayment.unapply))

  implicit val paymentReads: Reads[SignedPayment] = (
    (JsPath \ "timestamp").read[Long] and
      (JsPath \ "amount").read[Long] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "recipient").read[String] and
      (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "sender").read[String] and
      (JsPath \ "signature").read[String]
    ) (SignedPayment.apply _)
}
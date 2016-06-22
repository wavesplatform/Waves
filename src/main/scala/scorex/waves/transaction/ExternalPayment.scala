package scorex.waves.transaction

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Reads, Writes}

case class ExternalPayment(timestamp: Long, amount: Long, fee: Long, senderPublicKey: String, recipient: String, signature: String)


object ExternalPayment {
  implicit val paymentWrites: Writes[ExternalPayment] = (
    (JsPath \ "timestamp").write[Long] and
      (JsPath \ "amount").write[Long] and
      (JsPath \ "fee").write[Long] and
      (JsPath \ "senderPublicKey").write[String] and
      (JsPath \ "recipient").write[String] and
      (JsPath \ "signature").write[String]
    ) (unlift(ExternalPayment.unapply))

  implicit val paymentReads: Reads[ExternalPayment] = (
    (JsPath \ "timestamp").read[Long] and
      (JsPath \ "amount").read[Long] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "recipient").read[String] and
      (JsPath \ "signature").read[String]
    ) (ExternalPayment.apply _)
}

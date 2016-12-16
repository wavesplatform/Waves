package scorex.waves.transaction

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Reads, Writes}
import scorex.account.{Account, PublicKeyAccount}
import scorex.api.http.formats._

case class ExternalPayment(timestamp: Long, amount: Long, fee: Long, senderPublicKey: PublicKeyAccount, recipient: Account, signature: Array[Byte])


object ExternalPayment {
  implicit val paymentWrites: Writes[ExternalPayment] = (
    (JsPath \ "timestamp").write[Long] and
      (JsPath \ "amount").write[Long] and
      (JsPath \ "fee").write[Long] and
      (JsPath \ "senderPublicKey").write[PublicKeyAccount](PublicKeyAccountWrites) and
      (JsPath \ "recipient").write[Account] and
      (JsPath \ "signature").write[Array[Byte]]
    ) (unlift(ExternalPayment.unapply))

  implicit val paymentReads: Reads[ExternalPayment] = (
    (JsPath \ "timestamp").read[Long] and
      (JsPath \ "amount").read[Long] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "senderPublicKey").read[PublicKeyAccount] and
      (JsPath \ "recipient").read[Account] and
      (JsPath \ "signature").read[Array[Byte]]
    ) (ExternalPayment.apply _)
}

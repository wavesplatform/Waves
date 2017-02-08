package scorex.waves.transaction

import play.api.libs.functional.syntax._
import play.api.libs.json._
import scorex.account.{Account, PublicKeyAccount}
import scorex.api.http.formats._

case class ExternalPayment(timestamp: Long, amount: Long, fee: Long, senderPublicKey: PublicKeyAccount, recipient: Account, signature: Array[Byte])


object ExternalPayment {
  implicit val paymentFormat: Format[ExternalPayment] = (
    (__ \ "timestamp").format[Long] ~
      (__ \ "amount").format[Long] ~
      (__ \ "fee").format[Long] ~
      (__ \ "senderPublicKey").format[PublicKeyAccount] ~
      (__ \ "recipient").format[Account] ~
      (__ \ "signature").format[Array[Byte]]
    ) (ExternalPayment.apply, unlift(ExternalPayment.unapply))
}

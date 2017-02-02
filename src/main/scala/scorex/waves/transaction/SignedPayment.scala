package scorex.waves.transaction

import play.api.libs.functional.syntax._
import play.api.libs.json._
import scorex.account.{Account, PublicKeyAccount}
import scorex.api.http.formats._

case class SignedPayment(timestamp: Long,
                         amount: Long,
                         fee: Long,
                         recipient: Account,
                         senderPublicKey: PublicKeyAccount,
                         sender: String,
                         signature: String)

object SignedPayment {
  implicit val paymentFormat: Format[SignedPayment] = (
    (__ \ "timestamp").format[Long] and
    (__ \ "amount").format[Long] and
    (__ \ "fee").format[Long] and
    (__ \ "recipient").format[Account] and
    (__ \ "senderPublicKey").format[PublicKeyAccount](PublicKeyAccountWrites) and
    (__ \ "sender").format[String] and
    (__ \ "signature").format[String]) (SignedPayment.apply, unlift(SignedPayment.unapply))
}

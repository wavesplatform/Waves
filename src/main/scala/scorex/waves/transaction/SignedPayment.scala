package scorex.waves.transaction

import play.api.libs.json._

case class SignedPayment(
    timestamp: Long,
    amount: Long,
    fee: Long,
    recipient: String,
    senderPublicKey: String,
    sender: String,
    signature: String)

object SignedPayment {
  implicit val paymentFormat: Format[SignedPayment] = Json.format
}

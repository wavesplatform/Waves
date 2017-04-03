package scorex.waves.transaction

import play.api.libs.json._

case class SignedPaymentRequest(
    timestamp: Long,
    amount: Long,
    fee: Long,
    recipient: String,
    senderPublicKey: String,
    sender: String,
    signature: String)

object SignedPaymentRequest {
  implicit val paymentFormat: Format[SignedPaymentRequest] = Json.format
}

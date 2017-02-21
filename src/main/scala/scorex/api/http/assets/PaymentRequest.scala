package scorex.api.http.assets

import play.api.libs.json.{Format, Json}

case class PaymentRequest(amount: Long, fee: Long, sender: String, recipient: String)

object PaymentRequest {
  implicit val paymentFormat: Format[PaymentRequest] = Json.format
}

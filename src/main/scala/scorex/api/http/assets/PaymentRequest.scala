package scorex.transaction.state.wallet

import play.api.libs.json.{Format, Json}

case class Payment(amount: Long, fee: Long, sender: String, recipient: String)

object Payment {
  implicit val paymentFormat: Format[Payment] = Json.format
}

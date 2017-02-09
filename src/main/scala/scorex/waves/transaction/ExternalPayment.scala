package scorex.waves.transaction

import play.api.libs.json._
import scorex.account.{Account, PublicKeyAccount}

case class ExternalPayment(timestamp: Long, amount: Long, fee: Long, senderPublicKey: PublicKeyAccount, recipient: Account, signature: Array[Byte])


object ExternalPayment {
  import scorex.api.http.formats._

  implicit val paymentFormat: Format[ExternalPayment] = Json.format
}

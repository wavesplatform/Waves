package scorex.waves.http

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Reads, Writes}

case class UnsignedPayment(timestamp: Long,
                         amount: Long,
                         fee: Long,
                         recipient: String,
                         senderWalletSeed: String,
                         senderAddressNonce: Int)

object UnsignedPayment {
  implicit val paymentReads: Reads[UnsignedPayment] = (
    (JsPath \ "timestamp").read[Long] and
      (JsPath \ "amount").read[Long] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "recipient").read[String] and
      (JsPath \ "senderWalletSeed").read[String] and
      (JsPath \ "senderAddressNonce").read[Int]
    ) (UnsignedPayment.apply _)
}
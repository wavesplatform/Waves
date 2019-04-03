package com.wavesplatform.http

import play.api.libs.json.{Format, Json}

case class UnsignedPayment(timestamp: Long, amount: Long, fee: Long, recipient: String, senderWalletSeed: String, senderAddressNonce: Int)

object UnsignedPayment {
  implicit val paymentReads: Format[UnsignedPayment] = Json.format
}

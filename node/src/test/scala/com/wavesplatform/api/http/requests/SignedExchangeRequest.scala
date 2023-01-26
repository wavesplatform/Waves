package com.wavesplatform.api.http.requests

import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.assets.exchange.*
import play.api.libs.json.{Format, Json, Reads}

object SignedExchangeRequest {
  implicit val orderReads: Reads[Order]                                   = com.wavesplatform.transaction.assets.exchange.OrderJson.orderReads
  implicit val signedExchangeRequestFormat: Format[SignedExchangeRequest] = Json.format
}

case class SignedExchangeRequest(
    senderPublicKey: String,
    order1: Order,
    order2: Order,
    amount: Long,
    price: Long,
    fee: Long,
    buyMatcherFee: Long,
    sellMatcherFee: Long,
    timestamp: Long,
    signature: String
) {
  def toTx: Either[ValidationError, ExchangeTransaction] =
    for {
      _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
      _t <- ExchangeTransaction.create(1.toByte, order1, order2, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, Proofs(_signature))
    } yield _t

}

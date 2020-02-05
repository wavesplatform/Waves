package com.wavesplatform.api.http.assets

import com.wavesplatform.account.PublicKey
import com.wavesplatform.api.http.BroadcastRequest
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.TransactionParsers.SignatureStringLength
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.exchange._
import play.api.libs.json.{Format, Json}

object SignedExchangeRequest {
  implicit val orderFormat: Format[Order]                                 = com.wavesplatform.transaction.assets.exchange.OrderJson.orderFormat
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
) extends BroadcastRequest {
  def toTx: Either[ValidationError, ExchangeTransaction] =
    for {
      _sender    <- PublicKey.fromBase58String(senderPublicKey)
      _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
      o1         <- castOrder(order1)
      o2         <- castOrder(order2)
      _t         <- ExchangeTransactionV1.create(o1, o2, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, _signature)
    } yield _t

  def castOrder(o: Order): Either[ValidationError, OrderV1] = o match {
    case o1 @ OrderV1(_, _, _, _, _, _, _, _, _, _) => Right(o1)
    case _                                          => Left(GenericError("ExchangeTransaction of version 1 can only contain orders of version 1"))
  }
}

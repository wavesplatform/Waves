package com.wavesplatform.api.http.requests

import com.wavesplatform.account.PublicKey
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order}
import com.wavesplatform.transaction.{TxAmount, TxTimestamp, TxVersion}
import play.api.libs.json.Json

case class ExchangeRequest(
    order1: Order,
    order2: Order,
    amount: Long,
    price: Long,
    buyMatcherFee: Long,
    sellMatcherFee: Long,
    version: Option[TxVersion] = None,
    sender: Option[String] = None,
    senderPublicKey: Option[String] = None,
    fee: Option[TxAmount] = None,
    timestamp: Option[TxTimestamp] = None,
    signature: Option[String] = None,
    proofs: Option[List[String]] = None
) extends TxBroadcastRequest {
  def toTxFrom(sender: PublicKey): Either[ValidationError, ExchangeTransaction] =
    for {
      validProofs <- toProofs(version, signature, proofs)
      tx <- ExchangeTransaction.create(
        version.getOrElse(1.toByte),
        order1,
        order2,
        amount,
        price,
        buyMatcherFee,
        sellMatcherFee,
        fee.getOrElse(0L),
        timestamp.getOrElse(0L),
        validProofs
      )
    } yield tx
}

object ExchangeRequest {
  implicit val jsonFormat = Json.format[ExchangeRequest]
}

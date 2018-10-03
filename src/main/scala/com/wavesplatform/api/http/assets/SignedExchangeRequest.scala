package com.wavesplatform.api.http.assets

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.api.http.BroadcastRequest
import com.wavesplatform.transaction.TransactionParsers.SignatureStringLength
import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, ExchangeTransactionV1, Order, OrderV1}

object SignedExchangeRequest {
  implicit val orderFormat: Format[Order]                                 = com.wavesplatform.transaction.assets.exchange.OrderJson.orderFormat
  implicit val signedExchangeRequestFormat: Format[SignedExchangeRequest] = Json.format
}

case class SignedExchangeRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                 senderPublicKey: String,
                                 @ApiModelProperty(value = "Buy Order")
                                 order1: Order,
                                 @ApiModelProperty(value = "Sell Order")
                                 order2: Order,
                                 @ApiModelProperty(required = true)
                                 price: Long,
                                 @ApiModelProperty(required = true, example = "1000000")
                                 amount: Long,
                                 @ApiModelProperty(required = true)
                                 fee: Long,
                                 @ApiModelProperty(required = true)
                                 buyMatcherFee: Long,
                                 @ApiModelProperty(required = true)
                                 sellMatcherFee: Long,
                                 @ApiModelProperty(required = true)
                                 timestamp: Long,
                                 @ApiModelProperty(required = true)
                                 signature: String)
    extends BroadcastRequest {
  def toTx: Either[ValidationError, ExchangeTransaction] =
    for {
      _sender    <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
      _t <- ExchangeTransactionV1.create(order1.asInstanceOf[OrderV1],
                                         order2.asInstanceOf[OrderV1],
                                         price,
                                         amount,
                                         buyMatcherFee,
                                         sellMatcherFee,
                                         fee,
                                         timestamp,
                                         _signature)
    } yield _t
}

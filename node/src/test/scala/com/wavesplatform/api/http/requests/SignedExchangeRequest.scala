package com.wavesplatform.api.http.requests

import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.assets.exchange._
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

object SignedExchangeRequest {
  implicit val orderFormat: Format[Order]                                 = com.wavesplatform.transaction.assets.exchange.OrderJson.orderFormat
  implicit val signedExchangeRequestFormat: Format[SignedExchangeRequest] = Json.format
}

case class SignedExchangeRequest(
    @ApiModelProperty(value = "Base58 encoded sender public key", required = true)
    senderPublicKey: String,
    @ApiModelProperty(value = "Buy Order")
    order1: Order,
    @ApiModelProperty(value = "Sell Order")
    order2: Order,
    @ApiModelProperty(required = true, example = "1000000")
    amount: Long,
    @ApiModelProperty(required = true)
    price: Long,
    @ApiModelProperty(required = true)
    fee: Long,
    @ApiModelProperty(required = true)
    buyMatcherFee: Long,
    @ApiModelProperty(required = true)
    sellMatcherFee: Long,
    @ApiModelProperty(required = true)
    timestamp: Long,
    @ApiModelProperty(required = true)
    signature: String
) {
  def toTx: Either[ValidationError, ExchangeTransaction] =
    for {
      _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
      _t         <- ExchangeTransaction.create(1.toByte, order1, order2, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, Proofs(_signature))
    } yield _t

}

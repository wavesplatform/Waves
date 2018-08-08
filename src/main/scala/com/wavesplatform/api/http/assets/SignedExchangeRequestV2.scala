package com.wavesplatform.api.http.assets

import cats.implicits._
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.api.http.BroadcastRequest
import com.wavesplatform.transaction.{ValidationError, Proofs}
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, ExchangeTransactionV2, Order}

object SignedExchangeRequestV2 {
  implicit val orderFormat: Format[Order]                                   = com.wavesplatform.transaction.assets.exchange.OrderJson.orderFormat
  implicit val signedExchangeRequestFormat: Format[SignedExchangeRequestV2] = Json.format
}

case class SignedExchangeRequestV2(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
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
                                   @ApiModelProperty(required = true, example = "2")
                                   version: Byte,
                                   @ApiModelProperty(required = true)
                                   proofs: List[String],
) extends BroadcastRequest {
  def toTx: Either[ValidationError, ExchangeTransaction] =
    for {
      _sender     <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      _t          <- ExchangeTransactionV2.create(order1, order2, price, amount, buyMatcherFee, sellMatcherFee, fee, timestamp, _proofs)
    } yield _t
}

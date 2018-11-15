package com.wavesplatform.api.http.assets

import cats.implicits._
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.api.http.BroadcastRequest
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, ExchangeTransactionV2, Order}
import com.wavesplatform.transaction.{Proofs, ValidationError}
import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json.{Format, JsPath, Reads}

object SignedExchangeRequestV2 {
  implicit val orderFormat: Format[Order] = com.wavesplatform.transaction.assets.exchange.OrderJson.orderFormat

  implicit val signedExchangeRequestReads: Reads[SignedExchangeRequestV2] = (
    (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "order1").read[Order] and
      (JsPath \ "order2").read[Order] and
      (JsPath \ "price").read[Long] and
      (JsPath \ "amount").read[Long] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "buyMatcherFee").read[Long] and
      (JsPath \ "sellMatcherFee").read[Long] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "version").read[Byte] and
      (JsPath \ "proofs").read[List[ProofStr]]
  )(SignedExchangeRequestV2.apply _)
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
      _t          <- ExchangeTransactionV2.create(order1, order2, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, _proofs)
    } yield _t
}

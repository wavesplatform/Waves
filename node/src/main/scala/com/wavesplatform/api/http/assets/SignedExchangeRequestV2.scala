package com.wavesplatform.api.http.assets

import cats.implicits._
import com.wavesplatform.account.PublicKey
import com.wavesplatform.api.http.BroadcastRequest
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, ExchangeTransactionV2, Order}
import com.wavesplatform.transaction.Proofs
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

case class SignedExchangeRequestV2(senderPublicKey: String,
                                   order1: Order,
                                   order2: Order,
                                   price: Long,
                                   amount: Long,
                                   fee: Long,
                                   buyMatcherFee: Long,
                                   sellMatcherFee: Long,
                                   timestamp: Long,
                                   version: Byte,
                                   proofs: List[String],
) extends BroadcastRequest {
  def toTx: Either[ValidationError, ExchangeTransaction] =
    for {
      _sender     <- PublicKey.fromBase58String(senderPublicKey)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      _t          <- ExchangeTransactionV2.create(order1, order2, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, _proofs)
    } yield _t
}

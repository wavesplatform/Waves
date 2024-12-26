package com.wavesplatform.api.http.requests

import cats.instances.list.*
import cats.syntax.traverse.*
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order}
import play.api.libs.functional.syntax.*
import play.api.libs.json.{JsPath, Reads}

object SignedExchangeRequestV2 {
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

case class SignedExchangeRequestV2(
    senderPublicKey: String,
    order1: Order,
    order2: Order,
    price: Long,
    amount: Long,
    fee: Long,
    buyMatcherFee: Long,
    sellMatcherFee: Long,
    timestamp: Long,
    version: Byte,
    proofs: List[String]
) {
  def toTx: Either[ValidationError, ExchangeTransaction] =
    for {
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      _t          <- ExchangeTransaction.create(2.toByte, order1, order2, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, _proofs)
    } yield _t
}

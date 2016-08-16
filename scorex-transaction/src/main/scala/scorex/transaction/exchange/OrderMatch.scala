package scorex.transaction.exchange

import play.api.libs.json.JsObject
import scorex.crypto.EllipticCurveImpl
import scorex.serialization.BytesSerializable
import scorex.transaction.Transaction

case class OrderMatch(order1: Order, order2: Order, price: Long, amount: Long, matcherFee: Long, fee: Long,
                      timestamp: Long, signature: Array[Byte]) extends Transaction with BytesSerializable {

  override def json: JsObject = ???

  def isValid(previousMatches: Seq[OrderMatch]): Boolean = {
    lazy val order1Transactions = previousMatches.filter { om =>
      (om.order1.signature sameElements order1.signature) || (om.order2.signature sameElements order1.signature)
    }
    lazy val order2Transactions = previousMatches.filter { om =>
      (om.order1.signature sameElements order2.signature) || (om.order2.signature sameElements order2.signature)
    }

    lazy val ordersMatches: Boolean = {
      val priceMatches = if (order1.priceAssetId sameElements order1.receiveAssetId) order1.price <= order2.price
      else order2.price >= order1.price
      (order1.matcher.address == order2.matcher.address) &&
        (order1.spendAssetId sameElements order2.receiveAssetId) &&
        (order2.spendAssetId sameElements order1.receiveAssetId) && priceMatches
    }.ensuring(a => !a || (order1.amountAsset sameElements order2.amountAsset))
    lazy val priceIsValid: Boolean = (order1.price == price) || (order2.price == price)
    lazy val amountIsValid: Boolean = {
      lazy val order1Total = order1Transactions.map(_.amount).sum + amount
      lazy val order2Total = order2Transactions.map(_.amount).sum + amount
      (order1Total <= order1.amount) && (order2Total <= order2.amount)
    }
    lazy val matcherFeeIsValid: Boolean = {
      //TODO Matcher takes all his fee on his first match and takes nothing after that
      val o1maxFee = if (order1Transactions.isEmpty) order1.matcherFee else 0
      val o2maxFee = if (order2Transactions.isEmpty) order2.matcherFee else 0
      matcherFee <= (o1maxFee + o2maxFee)
    }
    lazy val matcherSignatureIsValid: Boolean =
      EllipticCurveImpl.verify(signature, toSign, order1.matcher.publicKey)

    amount > 0 && price > 0 && ordersMatches && order1.isValid && order2.isValid && priceIsValid && amountIsValid &&
      matcherFeeIsValid && matcherSignatureIsValid
  }

  lazy val toSign: Array[Byte] = ???

  override def bytes: Array[Byte] = toSign ++ signature
}


package scorex.transaction.exchange

import scorex.crypto.EllipticCurveImpl
import scorex.serialization.BytesSerializable

case class OrderMatch(order1: Order, order2: Order, price: Long, amount: Long, matcherFee: Long, fee: Long,
                      timestamp: Long, matcherSignature: Array[Byte])
  extends BytesSerializable {

  def isValid(previousMatches: Seq[OrderMatch]): Boolean = {
    lazy val order1Transactions = previousMatches.filter { om =>
      (om.order1.signature sameElements order1.signature) || (om.order2.signature sameElements order1.signature)
    }
    lazy val order2Transactions = previousMatches.filter { om =>
      (om.order1.signature sameElements order2.signature) || (om.order2.signature sameElements order2.signature)
    }

    lazy val ordersMatches: Boolean = {
      val priceMatches = if (order1.priceAssetId == order1.receiveAssetID) order1.price <= order2.price
      else order2.price >= order1.price
      (order1.matcherAddress.address == order2.matcherAddress.address) &&
        (order1.spendAssetID sameElements order2.receiveAssetID) &&
        (order2.spendAssetID sameElements order1.receiveAssetID) && priceMatches
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
      EllipticCurveImpl.verify(matcherSignature, toSign, order1.matcherAddress.publicKey)

    ordersMatches && order1.isValid && order2.isValid && priceIsValid && amountIsValid && matcherFeeIsValid &&
      matcherSignatureIsValid
  }

  lazy val toSign: Array[Byte] = ???

  override def bytes: Array[Byte] = toSign ++ matcherSignature
}


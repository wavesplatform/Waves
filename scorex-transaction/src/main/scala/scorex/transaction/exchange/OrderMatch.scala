package scorex.transaction.exchange

import com.google.common.primitives.{Ints, Longs}
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.serialization.{BytesSerializable, Deser}
import scorex.transaction.Transaction

import scala.util.Try

case class OrderMatch(order1: Order, order2: Order, price: Long, amount: Long, matcherFee: Long, fee: Long,
                      timestamp: Long, signature: Array[Byte]) extends Transaction with BytesSerializable {

  override def json: JsObject = Json.obj(
    "order1" -> order1.json,
    "order2" -> order2.json,
    "price" -> price,
    "amount" -> amount,
    "matcherFee" -> matcherFee,
    "fee" -> fee,
    "timestamp" -> timestamp,
    "signature" -> Base58.encode(signature)
  )

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

  lazy val toSign: Array[Byte] = Ints.toByteArray(order1.bytes.length) ++ Ints.toByteArray(order2.bytes.length) ++
    order1.bytes ++ order2.bytes ++ Longs.toByteArray(price) ++ Longs.toByteArray(amount) ++
    Longs.toByteArray(matcherFee) ++ Longs.toByteArray(fee) ++ Longs.toByteArray(timestamp)

  override def bytes: Array[Byte] = toSign ++ signature
}

object OrderMatch extends Deser[OrderMatch] {
  override def parseBytes(bytes: Array[Byte]): Try[OrderMatch] = Try {
    val o1Size = Ints.fromByteArray(bytes.slice(0, 4))
    val o2Size = Ints.fromByteArray(bytes.slice(4, 8))
    val o1 = Order.parseBytes(bytes.slice(8, 8 + o1Size)).get
    val o2 = Order.parseBytes(bytes.slice(8 + o1Size, 8 + o1Size + o2Size)).get
    val s = 8 + o1Size + o2Size
    val price = Longs.fromByteArray(bytes.slice(s, s + 8))
    val amount = Longs.fromByteArray(bytes.slice(s + 8, s + 16))
    val matcherFee = Longs.fromByteArray(bytes.slice(s + 16, s + 24))
    val fee = Longs.fromByteArray(bytes.slice(s + 24, s + 32))
    val timestamp = Longs.fromByteArray(bytes.slice(s + 32, s + 40))
    val signature = bytes.slice(s + 40, bytes.length)
    OrderMatch(o1, o2, price, amount, matcherFee, fee, timestamp, signature)
  }
}

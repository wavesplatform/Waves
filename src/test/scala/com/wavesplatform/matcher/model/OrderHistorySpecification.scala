package com.wavesplatform.matcher.model

import com.wavesplatform.TestDB
import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.matcher.model.Events.{OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.state2.ByteStr
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.account.PrivateKeyAccount
import scorex.transaction.AssetAcc
import scorex.transaction.assets.exchange.AssetPair

class OrderHistorySpecification extends PropSpec
  with TestDB
  with PropertyChecks
  with Matchers
  with MatcherTestData
  with BeforeAndAfterAll
  with BeforeAndAfterEach {

  val pair = AssetPair(Some(ByteStr("WCT".getBytes)), Some(ByteStr("BTC".getBytes)))
  val db = open()
  var oh = OrderHistoryImpl(db)

  override protected def beforeEach(): Unit = {
    oh = OrderHistoryImpl(open())
  }

  property("New buy order added") {
    val ord1 = buy(pair, 0.0007, 10000)

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.orderStatus(ord1.idStr()) shouldBe LimitOrder.Accepted
    oh.orderInfo(ord1.idStr()) shouldBe OrderInfo(ord1.amount, 0, false)

    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe 0L
    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 7L
    oh.openVolume(AssetAcc(ord1.senderPublicKey, None)) shouldBe ord1.matcherFee

    oh.ordersByPairAndAddress(pair, ord1.senderPublicKey.address) shouldBe Set(ord1.idStr())
  }

  property("New sell order added") {
    val ord1 = sell(pair, 0.0007, 10000)

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.orderStatus(ord1.idStr()) shouldBe LimitOrder.Accepted
    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe 10000L
    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 0L
    oh.openVolume(AssetAcc(ord1.senderPublicKey, None)) shouldBe ord1.matcherFee

    oh.ordersByPairAndAddress(pair, ord1.senderPublicKey.address) shouldBe Set(ord1.idStr())
  }

  property("New buy WAVES order added") {
    val pair = AssetPair(None, Some(ByteStr("BTC".getBytes)))
    val ord1 = buy(pair, 0.0008, 10000)

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.orderStatus(ord1.idStr()) shouldBe LimitOrder.Accepted
    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe
      math.max(ord1.matcherFee - ord1.getReceiveAmount(ord1.price, ord1.amount).right.get, 0L)
    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 8L

    oh.ordersByPairAndAddress(pair, ord1.senderPublicKey.address) shouldBe Set(ord1.idStr())
  }

  property("New sell WAVES order added") {
    val pair = AssetPair(None, Some(ByteStr("BTC".getBytes)))
    val ord1 = sell(pair, 0.0008, 10000)

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.orderStatus(ord1.idStr()) shouldBe LimitOrder.Accepted
    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe 10000L + ord1.matcherFee
    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 0L

    oh.ordersByPairAndAddress(pair, ord1.senderPublicKey.address) shouldBe Set(ord1.idStr())
  }

  property("New buy and sell WAVES order  added") {
    val pk = PrivateKeyAccount("private".getBytes("utf-8"))
    val pair = AssetPair(None, Some(ByteStr("BTC".getBytes)))
    val ord1 = buy(pair, 0.0008, 100000000, Some(pk))
    val ord2 = sell(pair, 0.0009, 210000000, Some(pk))

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.orderAccepted(OrderAdded(LimitOrder(ord2)))

    oh.orderStatus(ord1.idStr()) shouldBe LimitOrder.Accepted
    oh.orderStatus(ord2.idStr()) shouldBe LimitOrder.Accepted

    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe
      math.max(ord1.matcherFee - ord1.getReceiveAmount(ord1.price, ord1.amount).right.get, 0L) + ord2.amount + ord2.matcherFee
    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe
      ord1.getSpendAmount(ord1.price, ord1.amount).right.get

    oh.ordersByPairAndAddress(pair, ord1.senderPublicKey.address) shouldBe Set(ord1.idStr(), ord2.idStr())
  }

  property("Buy WAVES order filled") {
    val pair = AssetPair(None, Some(ByteStr("BTC".getBytes)))
    val ord1 = buy(pair, 0.0008, 10000)
    val ord2 = sell(pair, 0.0007, 10000)

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.orderExecuted(OrderExecuted(LimitOrder(ord2), LimitOrder(ord1)))

    oh.orderStatus(ord1.idStr()) shouldBe LimitOrder.Filled
    oh.orderStatus(ord2.idStr()) shouldBe LimitOrder.Filled

    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe 0L
    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 0L
    oh.ordersByPairAndAddress(pair, ord1.senderPublicKey.address) shouldBe Set(ord1.idStr())

    oh.openVolume(AssetAcc(ord2.senderPublicKey, pair.amountAsset)) shouldBe 0L
    oh.openVolume(AssetAcc(ord2.senderPublicKey, pair.priceAsset)) shouldBe 0L
    oh.ordersByPairAndAddress(pair, ord2.senderPublicKey.address) shouldBe Set(ord2.idStr())

  }

  property("Sell WAVES order - filled, buy order - partial") {
    val pair = AssetPair(None, Some(ByteStr("BTC".getBytes)))
    val ord1 = sell(pair, 0.0008, 100000000)
    val ord2 = buy(pair, 0.00085, 120000000)

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    val exec = OrderExecuted(LimitOrder(ord2), LimitOrder(ord1))
    oh.orderExecuted(exec)
    oh.orderAccepted(OrderAdded(exec.submittedRemainingOrder))

    oh.orderStatus(ord1.idStr()) shouldBe LimitOrder.Filled
    oh.orderStatus(ord2.idStr()) shouldBe LimitOrder.PartiallyFilled(100000000)

    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe 0L
    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 0L
    oh.ordersByPairAndAddress(pair, ord1.senderPublicKey.address) shouldBe Set(ord1.idStr())

    oh.openVolume(AssetAcc(ord2.senderPublicKey, pair.amountAsset)) shouldBe
      math.max(0L, OrderInfo.safeSum(ord2.matcherFee * 2 / 12, -20000000L))
    oh.openVolume(AssetAcc(ord2.senderPublicKey, pair.priceAsset)) shouldBe 0.00085 * 20000000L
    oh.ordersByPairAndAddress(pair, ord2.senderPublicKey.address) shouldBe Set(ord2.idStr())

  }

  property("Buy WAVES order - filled with 2 steps, sell order - partial") {
    val pair = AssetPair(None, Some(ByteStr("BTC".getBytes)))
    val ord1 = buy(pair, 0.0008, 100000000, matcherFee = Some(300001L))
    val ord2 = sell(pair, 0.00075, 50000000, matcherFee = Some(300001L))
    val ord3 = sell(pair, 0.0008, 80000000, matcherFee = Some(300001L))

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    val exec1 = OrderExecuted(LimitOrder(ord2), LimitOrder(ord1))
    oh.orderExecuted(exec1)

    oh.orderStatus(ord1.idStr()) shouldBe LimitOrder.PartiallyFilled(50000000)
    oh.orderStatus(ord2.idStr()) shouldBe LimitOrder.Filled

    val exec2 = OrderExecuted(LimitOrder(ord3), exec1.counterRemainingOrder)
    oh.orderExecuted(exec2)
    oh.orderAccepted(OrderAdded(exec2.submittedRemainingOrder))

    oh.orderStatus(ord1.idStr()) shouldBe LimitOrder.Filled
    oh.orderStatus(ord2.idStr()) shouldBe LimitOrder.Filled

    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 0L
    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe 0L
    oh.ordersByPairAndAddress(pair, ord1.senderPublicKey.address) shouldBe Set(ord1.idStr())


    oh.openVolume(AssetAcc(ord2.senderPublicKey, pair.priceAsset)) shouldBe 0L
    oh.openVolume(AssetAcc(ord2.senderPublicKey, pair.amountAsset)) shouldBe 0L
    oh.ordersByPairAndAddress(pair, ord2.senderPublicKey.address) shouldBe Set(ord2.idStr())

    oh.openVolume(AssetAcc(ord3.senderPublicKey, pair.amountAsset)) shouldBe ord3.matcherFee * 3 / 8 + 30000000L
    oh.openVolume(AssetAcc(ord3.senderPublicKey, pair.priceAsset)) shouldBe 0L
    oh.ordersByPairAndAddress(pair, ord3.senderPublicKey.address) shouldBe Set(ord3.idStr())

  }

  property("Partially with own order") {
    val pk = PrivateKeyAccount("private".getBytes("utf-8"))
    val pair = AssetPair(None, Some(ByteStr("BTC".getBytes)))
    val ord1 = buy(pair, 0.0008, 100000000, Some(pk), Some(300000L))
    val ord2 = sell(pair, 0.00079, 210000000, Some(pk), Some(300000L))

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    val exec1 = OrderExecuted(LimitOrder(ord2), LimitOrder(ord1))
    oh.orderExecuted(exec1)
    oh.orderAccepted(OrderAdded(exec1.submittedRemainingOrder))

    oh.orderStatus(ord1.idStr()) shouldBe LimitOrder.Filled
    oh.orderStatus(ord2.idStr()) shouldBe LimitOrder.PartiallyFilled(100000000)

    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe 110000000L + ord2.matcherFee * 11 / 21
    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 0L
    oh.ordersByPairAndAddress(pair, ord1.senderPublicKey.address) shouldBe Set(ord1.idStr(), ord2.idStr())
  }

  property("Cancel buy order") {
    val ord1 = buy(pair, 0.0008, 100000000, matcherFee = Some(300000L))

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.orderCanceled(OrderCanceled(LimitOrder(ord1)))

    oh.orderStatus(ord1.idStr()) shouldBe LimitOrder.Cancelled(0)

    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe 0L
    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 0L
    oh.ordersByPairAndAddress(pair, ord1.senderPublicKey.address) shouldBe Set(ord1.idStr())
  }

  property("Cancel sell order") {
    val ord1 = sell(pair, 0.0008, 100000000, matcherFee = Some(300000L))

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.orderCanceled(OrderCanceled(LimitOrder(ord1)))

    oh.orderStatus(ord1.idStr()) shouldBe LimitOrder.Cancelled(0)

    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe 0L
    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 0L
    oh.ordersByPairAndAddress(pair, ord1.senderPublicKey.address) shouldBe Set(ord1.idStr())
  }

  property("Cancel partially executed order") {
    val pair = AssetPair(None, Some(ByteStr("BTC".getBytes)))
    val ord1 = sell(pair, 0.0008, 2100000000, matcherFee = Some(300000L))
    val ord2 = buy(pair, 0.00081, 1000000000, matcherFee = Some(300000L))

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    val exec1 = OrderExecuted(LimitOrder(ord2), LimitOrder(ord1))
    oh.orderExecuted(exec1)
    oh.orderCanceled(OrderCanceled(exec1.counterRemainingOrder))

    oh.orderStatus(ord1.idStr()) shouldBe LimitOrder.Cancelled(1000000000)
    oh.orderStatus(ord2.idStr()) shouldBe LimitOrder.Filled

    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe 0L
    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 0L
    oh.ordersByPairAndAddress(pair, ord1.senderPublicKey.address) shouldBe Set(ord1.idStr())
    oh.ordersByPairAndAddress(pair, ord2.senderPublicKey.address) shouldBe Set(ord2.idStr())
  }

  property("Delete order") {
    val pk = PrivateKeyAccount("private".getBytes("utf-8"))
    val pair = AssetPair(None, Some(ByteStr("BTC".getBytes)))
    val ord1 = buy(pair, 0.0008, 210000000, Some(pk), Some(300000L))
    val ord2 = sell(pair, 0.00079, 100000000, Some(pk), Some(300000L))

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    val exec1 = OrderExecuted(LimitOrder(ord2), LimitOrder(ord1))
    oh.orderExecuted(exec1)

    oh.orderStatus(ord1.idStr()) shouldBe LimitOrder.PartiallyFilled(100000000)
    oh.orderStatus(ord2.idStr()) shouldBe LimitOrder.Filled

    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe 0L
    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 0.0008 * 110000000L
    oh.ordersByPairAndAddress(pair, ord1.senderPublicKey.address) shouldBe Set(ord1.idStr(), ord2.idStr())

    oh.deleteOrder(ord1.assetPair, ord1.senderPublicKey.address, ord1.idStr()) shouldBe false
    oh.deleteOrder(ord2.assetPair, ord2.senderPublicKey.address, ord2.idStr()) shouldBe true

    oh.ordersByPairAndAddress(pair, ord1.senderPublicKey.address) shouldBe Set(ord1.idStr())
  }
}




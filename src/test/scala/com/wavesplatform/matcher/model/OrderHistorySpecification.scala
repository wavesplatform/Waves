package com.wavesplatform.matcher.model

import ch.qos.logback.classic.Level
import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.matcher.model.Events.{OrderAdded, OrderCanceled, OrderExecuted}
import org.h2.mvstore.MVStore
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.account.PrivateKeyAccount
import scorex.transaction.AssetAcc
import scorex.transaction.assets.exchange.AssetPair

class OrderHistorySpecification extends PropSpec
  with PropertyChecks
  with Matchers
  with MatcherTestData
  with BeforeAndAfterAll
  with BeforeAndAfterEach {

  val pair = AssetPair(Some("WCT".getBytes), Some("BTC".getBytes))
  var storage = new OrderHistoryStorage(new MVStore.Builder().open())
  var oh = OrderHistoryImpl(storage)

  var prevLogLevel = Level.INFO

  override protected def beforeEach(): Unit = {
    storage = new OrderHistoryStorage(new MVStore.Builder().open())
    oh = OrderHistoryImpl(storage)
  }

  property("New buy order added") {
    val ord1 = buy(pair, 0.0007, 10000)

    oh.didOrderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.getOrderStatus(ord1.idStr) shouldBe LimitOrder.Accepted
    oh.getOrderInfo(ord1.idStr) shouldBe OrderInfo(ord1.amount, ord1.timestamp, 0, false)

    oh.getOpenVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe 0L
    oh.getOpenVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 7L
    oh.getOpenVolume(AssetAcc(ord1.senderPublicKey, None)) shouldBe ord1.matcherFee

    oh.getOrdersByPairAndAddress(pair, ord1.senderPublicKey.address) shouldBe Set(ord1.idStr)
  }

  property("New sell order added") {
    val ord1 = sell(pair, 0.0007, 10000)

    oh.didOrderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.getOrderStatus(ord1.idStr) shouldBe LimitOrder.Accepted
    oh.getOpenVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe 10000L
    oh.getOpenVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 0L
    oh.getOpenVolume(AssetAcc(ord1.senderPublicKey, None)) shouldBe ord1.matcherFee

    oh.getOrdersByPairAndAddress(pair, ord1.senderPublicKey.address) shouldBe Set(ord1.idStr)
  }

  property("New buy WAVES order added") {
    val pair = AssetPair(None, Some("BTC".getBytes))
    val ord1 = buy(pair, 0.0008, 10000)

    oh.didOrderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.getOrderStatus(ord1.idStr) shouldBe LimitOrder.Accepted
    oh.getOpenVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe
      math.max(ord1.matcherFee - ord1.getReceiveAmount(ord1.price, ord1.amount).right.get, 0L)
    oh.getOpenVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 8L

    oh.getOrdersByPairAndAddress(pair, ord1.senderPublicKey.address) shouldBe Set(ord1.idStr)
  }

  property("New sell WAVES order added") {
    val pair = AssetPair(None, Some("BTC".getBytes))
    val ord1 = sell(pair, 0.0008, 10000)

    oh.didOrderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.getOrderStatus(ord1.idStr) shouldBe LimitOrder.Accepted
    oh.getOpenVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe 10000L + ord1.matcherFee
    oh.getOpenVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 0L

    oh.getOrdersByPairAndAddress(pair, ord1.senderPublicKey.address) shouldBe Set(ord1.idStr)
  }

  property("New buy and sell WAVES order  added") {
    val pk = PrivateKeyAccount("private".getBytes("utf-8"))
    val pair = AssetPair(None, Some("BTC".getBytes))
    val ord1 = buy(pair, 0.0008, 100000000, Some(pk))
    val ord2 = sell(pair, 0.0009, 210000000, Some(pk))

    oh.didOrderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.didOrderAccepted(OrderAdded(LimitOrder(ord2)))

    oh.getOrderStatus(ord1.idStr) shouldBe LimitOrder.Accepted
    oh.getOrderStatus(ord2.idStr) shouldBe LimitOrder.Accepted

    oh.getOpenVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe
      math.max(ord1.matcherFee - ord1.getReceiveAmount(ord1.price, ord1.amount).right.get, 0L) + ord2.amount + ord2.matcherFee
    oh.getOpenVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe
      ord1.getSpendAmount(ord1.price, ord1.amount).right.get

    oh.getOrdersByPairAndAddress(pair, ord1.senderPublicKey.address) shouldBe Set(ord1.idStr, ord2.idStr)
  }

  property("Buy WAVES order filled") {
    val pair = AssetPair(None, Some("BTC".getBytes))
    val ord1 = buy(pair, 0.0008, 10000)
    val ord2 = sell(pair, 0.0007, 10000)

    oh.didOrderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.didOrderExecuted(OrderExecuted(LimitOrder(ord2), LimitOrder(ord1)))

    oh.getOrderStatus(ord1.idStr) shouldBe LimitOrder.Filled
    oh.getOrderStatus(ord2.idStr) shouldBe LimitOrder.Filled

    oh.getOpenVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe 0L
    oh.getOpenVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 0L
    oh.getOrdersByPairAndAddress(pair, ord1.senderPublicKey.address) shouldBe Set(ord1.idStr)

    oh.getOpenVolume(AssetAcc(ord2.senderPublicKey, pair.amountAsset)) shouldBe 0L
    oh.getOpenVolume(AssetAcc(ord2.senderPublicKey, pair.priceAsset)) shouldBe 0L
    oh.getOrdersByPairAndAddress(pair, ord2.senderPublicKey.address) shouldBe Set(ord2.idStr)

  }

  property("Sell WAVES order - filled, buy order - partial") {
    val pair = AssetPair(None, Some("BTC".getBytes))
    val ord1 = sell(pair, 0.0008, 100000000)
    val ord2 = buy(pair, 0.00085, 120000000)

    oh.didOrderAccepted(OrderAdded(LimitOrder(ord1)))
    val exec = OrderExecuted(LimitOrder(ord2), LimitOrder(ord1))
    oh.didOrderExecuted(exec)
    oh.didOrderAccepted(OrderAdded(exec.submittedRemainingOrder))

    oh.getOrderStatus(ord1.idStr) shouldBe LimitOrder.Filled
    oh.getOrderStatus(ord2.idStr) shouldBe LimitOrder.PartiallyFilled(100000000)

    oh.getOpenVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe 0L
    oh.getOpenVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 0L
    oh.getOrdersByPairAndAddress(pair, ord1.senderPublicKey.address) shouldBe Set(ord1.idStr)

    oh.getOpenVolume(AssetAcc(ord2.senderPublicKey, pair.amountAsset)) shouldBe
      OpenPortfolio.limitSubstract(ord2.matcherFee*2/12, 20000000L)
    oh.getOpenVolume(AssetAcc(ord2.senderPublicKey, pair.priceAsset)) shouldBe 0.00085*20000000L
    oh.getOrdersByPairAndAddress(pair, ord2.senderPublicKey.address) shouldBe Set(ord2.idStr)

  }

  property("Buy WAVES order - filled with 2 steps, sell order - partial") {
    val pair = AssetPair(None, Some("BTC".getBytes))
    val ord1 = buy(pair, 0.0008, 100000000, matcherFee = Some(300001L))
    val ord2 = sell(pair, 0.00075, 50000000, matcherFee = Some(300001L))
    val ord3 = sell(pair, 0.0008, 80000000, matcherFee = Some(300001L))

    oh.didOrderAccepted(OrderAdded(LimitOrder(ord1)))
    val exec1 = OrderExecuted(LimitOrder(ord2), LimitOrder(ord1))
    oh.didOrderExecuted(exec1)

    oh.getOrderStatus(ord1.idStr) shouldBe LimitOrder.PartiallyFilled(50000000)
    oh.getOrderStatus(ord2.idStr) shouldBe LimitOrder.Filled

    val exec2 = OrderExecuted(LimitOrder(ord3),exec1.counterRemainingOrder)
    oh.didOrderExecuted(exec2)
    oh.didOrderAccepted(OrderAdded(exec2.submittedRemainingOrder))

    oh.getOrderStatus(ord1.idStr) shouldBe LimitOrder.Filled
    oh.getOrderStatus(ord2.idStr) shouldBe LimitOrder.Filled

    oh.getOpenVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 0L
    oh.getOpenVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe 0L
    oh.getOrdersByPairAndAddress(pair, ord1.senderPublicKey.address) shouldBe Set(ord1.idStr)


    oh.getOpenVolume(AssetAcc(ord2.senderPublicKey, pair.priceAsset)) shouldBe 0L
    oh.getOpenVolume(AssetAcc(ord2.senderPublicKey, pair.amountAsset)) shouldBe 0L
    oh.getOrdersByPairAndAddress(pair, ord2.senderPublicKey.address) shouldBe Set(ord2.idStr)

    oh.getOpenVolume(AssetAcc(ord3.senderPublicKey, pair.amountAsset)) shouldBe ord3.matcherFee*3/8 + 30000000L
    oh.getOpenVolume(AssetAcc(ord3.senderPublicKey, pair.priceAsset)) shouldBe 0L
    oh.getOrdersByPairAndAddress(pair, ord3.senderPublicKey.address) shouldBe Set(ord3.idStr)

  }

  property("Partially will own order") {
    val pk = PrivateKeyAccount("private".getBytes("utf-8"))
    val pair = AssetPair(None, Some("BTC".getBytes))
    val ord1 = buy(pair, 0.0008, 100000000, Some(pk), Some(300000L))
    val ord2 = sell(pair, 0.00079, 210000000, Some(pk), Some(300000L))

    oh.didOrderAccepted(OrderAdded(LimitOrder(ord1)))
    val exec1 = OrderExecuted(LimitOrder(ord2), LimitOrder(ord1))
    oh.didOrderExecuted(exec1)
    oh.didOrderAccepted(OrderAdded(exec1.submittedRemainingOrder))

    oh.getOrderStatus(ord1.idStr) shouldBe LimitOrder.Filled
    oh.getOrderStatus(ord2.idStr) shouldBe LimitOrder.PartiallyFilled(100000000)

    oh.getOpenVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe 110000000L + ord2.matcherFee*11/21
    oh.getOpenVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 0L
    oh.getOrdersByPairAndAddress(pair, ord1.senderPublicKey.address) shouldBe Set(ord1.idStr, ord2.idStr)
  }

  property("Cancel buy order") {
    val ord1 = buy(pair, 0.0008, 100000000, matcherFee = Some(300000L))

    oh.didOrderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.didOrderCanceled(OrderCanceled(LimitOrder(ord1)))

    oh.getOrderStatus(ord1.idStr) shouldBe LimitOrder.Cancelled(0)

    oh.getOpenVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe 0L
    oh.getOpenVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 0L
    oh.getOrdersByPairAndAddress(pair, ord1.senderPublicKey.address) shouldBe Set(ord1.idStr)
  }

  property("Cancel sell order") {
    val ord1 = sell(pair, 0.0008, 100000000, matcherFee = Some(300000L))

    oh.didOrderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.didOrderCanceled(OrderCanceled(LimitOrder(ord1)))

    oh.getOrderStatus(ord1.idStr) shouldBe LimitOrder.Cancelled(0)

    oh.getOpenVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe 0L
    oh.getOpenVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 0L
    oh.getOrdersByPairAndAddress(pair, ord1.senderPublicKey.address) shouldBe Set(ord1.idStr)
  }

  property("Cancel partially executed order") {
    val pair = AssetPair(None, Some("BTC".getBytes))
    val ord1 = sell(pair, 0.0008, 2100000000, matcherFee = Some(300000L))
    val ord2 = buy(pair, 0.00081, 1000000000, matcherFee = Some(300000L))

    oh.didOrderAccepted(OrderAdded(LimitOrder(ord1)))
    val exec1 = OrderExecuted(LimitOrder(ord2), LimitOrder(ord1))
    oh.didOrderExecuted(exec1)
    oh.didOrderCanceled(OrderCanceled(exec1.counterRemainingOrder))

    oh.getOrderStatus(ord1.idStr) shouldBe LimitOrder.Cancelled(1000000000)
    oh.getOrderStatus(ord2.idStr) shouldBe LimitOrder.Filled

    oh.getOpenVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe 0L
    oh.getOpenVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 0L
    oh.getOrdersByPairAndAddress(pair, ord1.senderPublicKey.address) shouldBe Set(ord1.idStr)
    oh.getOrdersByPairAndAddress(pair, ord2.senderPublicKey.address) shouldBe Set(ord2.idStr)
  }

  property("Delete order") {
    val pk = PrivateKeyAccount("private".getBytes("utf-8"))
    val pair = AssetPair(None, Some("BTC".getBytes))
    val ord1 = buy(pair, 0.0008, 210000000, Some(pk), Some(300000L))
    val ord2 = sell(pair, 0.00079, 100000000, Some(pk), Some(300000L))

    oh.didOrderAccepted(OrderAdded(LimitOrder(ord1)))
    val exec1 = OrderExecuted(LimitOrder(ord2), LimitOrder(ord1))
    oh.didOrderExecuted(exec1)

    oh.getOrderStatus(ord1.idStr) shouldBe LimitOrder.PartiallyFilled(100000000)
    oh.getOrderStatus(ord2.idStr) shouldBe LimitOrder.Filled

    oh.getOpenVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe 0L
    oh.getOpenVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 0.0008*110000000L
    oh.getOrdersByPairAndAddress(pair, ord1.senderPublicKey.address) shouldBe Set(ord1.idStr, ord2.idStr)

    oh.deleteOrder(ord1.assetPair, ord1.senderPublicKey.address, ord1.idStr) shouldBe false
    oh.deleteOrder(ord2.assetPair, ord2.senderPublicKey.address, ord2.idStr) shouldBe true

    oh.getOrdersByPairAndAddress(pair, ord1.senderPublicKey.address) shouldBe Set(ord1.idStr)
  }
}




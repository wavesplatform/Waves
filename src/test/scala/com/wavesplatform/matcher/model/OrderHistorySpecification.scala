package com.wavesplatform.matcher.model

import com.wavesplatform.WithDB
import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.matcher.model.Events.{OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.state.{ByteStr, EitherExt2}
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.transaction.AssetAcc
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}

import scala.collection.mutable

class OrderHistorySpecification
    extends PropSpec
    with WithDB
    with PropertyChecks
    with Matchers
    with MatcherTestData
    with BeforeAndAfterAll
    with BeforeAndAfterEach {

  val pair = AssetPair(Some(ByteStr("WCT".getBytes)), Some(ByteStr("BTC".getBytes)))
  var oh   = OrderHistoryImpl(db, matcherSettings)

  override def beforeEach(): Unit = {
    super.beforeEach()
    oh = OrderHistoryImpl(db, matcherSettings)
  }

  property("New buy order added") {
    val ord1 = buy(pair, 0.0007, 10000)

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.orderStatus(ord1.idStr()) shouldBe LimitOrder.Accepted
    oh.orderInfo(ord1.idStr()) shouldBe OrderInfo(ord1.amount, 0, false)

    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe 0L
    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 7L
    oh.openVolume(AssetAcc(ord1.senderPublicKey, None)) shouldBe ord1.matcherFee

    oh.allOrderIdsByAddress(ord1.senderPublicKey.address) shouldBe Set(ord1.idStr())
    oh.activeOrderIdsByAddress(ord1.senderPublicKey.address) shouldBe Set(pair.priceAsset -> ord1.idStr())
  }

  property("New sell order added") {
    val ord1 = sell(pair, 0.0007, 10000)

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.orderStatus(ord1.idStr()) shouldBe LimitOrder.Accepted
    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe 10000L
    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 0L
    oh.openVolume(AssetAcc(ord1.senderPublicKey, None)) shouldBe ord1.matcherFee

    oh.allOrderIdsByAddress(ord1.senderPublicKey.address) shouldBe Set(ord1.idStr())
    oh.activeOrderIdsByAddress(ord1.senderPublicKey.address) shouldBe Set(pair.amountAsset -> ord1.idStr())
  }

  property("New buy WAVES order added") {
    val pair = AssetPair(None, Some(ByteStr("BTC".getBytes)))
    val ord1 = buy(pair, 0.0008, 10000)

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.orderStatus(ord1.idStr()) shouldBe LimitOrder.Accepted
    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe
      math.max(ord1.matcherFee - ord1.getReceiveAmount(ord1.price, ord1.amount).explicitGet(), 0L)
    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 8L

    oh.allOrderIdsByAddress(ord1.senderPublicKey.address) shouldBe Set(ord1.idStr())
    oh.activeOrderIdsByAddress(ord1.senderPublicKey.address) shouldBe Set(pair.priceAsset -> ord1.idStr())
  }

  property("New sell WAVES order added") {
    val pair = AssetPair(None, Some(ByteStr("BTC".getBytes)))
    val ord1 = sell(pair, 0.0008, 10000)

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.orderStatus(ord1.idStr()) shouldBe LimitOrder.Accepted
    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe 10000L + ord1.matcherFee
    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 0L

    oh.allOrderIdsByAddress(ord1.senderPublicKey.address) shouldBe Set(ord1.idStr())
    oh.activeOrderIdsByAddress(ord1.senderPublicKey.address) shouldBe Set(pair.amountAsset -> ord1.idStr())
  }

  property("New buy and sell WAVES order added") {
    val pk   = PrivateKeyAccount("private".getBytes("utf-8"))
    val pair = AssetPair(None, Some(ByteStr("BTC".getBytes)))
    val ord1 = buy(pair, 0.0008, 100000000, Some(pk))
    val ord2 = sell(pair, 0.0009, 210000000, Some(pk))

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.orderAccepted(OrderAdded(LimitOrder(ord2)))

    oh.orderStatus(ord1.idStr()) shouldBe LimitOrder.Accepted
    oh.orderStatus(ord2.idStr()) shouldBe LimitOrder.Accepted

    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe
      math.max(ord1.matcherFee - ord1.getReceiveAmount(ord1.price, ord1.amount).explicitGet(), 0L) + ord2.amount + ord2.matcherFee
    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe
      ord1.getSpendAmount(ord1.price, ord1.amount).explicitGet()

    oh.allOrderIdsByAddress(ord1.senderPublicKey.address) shouldBe Set(ord1.idStr(), ord2.idStr())
    oh.activeOrderIdsByAddress(ord1.senderPublicKey.address) shouldBe Set(pair.priceAsset -> ord1.idStr(), pair.amountAsset -> ord2.idStr())
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
    oh.allOrderIdsByAddress(ord1.senderPublicKey.address) shouldBe Set(ord1.idStr())
    oh.activeOrderIdsByAddress(ord1.senderPublicKey.address) shouldBe empty

    oh.openVolume(AssetAcc(ord2.senderPublicKey, pair.amountAsset)) shouldBe 0L
    oh.openVolume(AssetAcc(ord2.senderPublicKey, pair.priceAsset)) shouldBe 0L
    oh.allOrderIdsByAddress(ord2.senderPublicKey.address) shouldBe Set(ord2.idStr())
    oh.activeOrderIdsByAddress(ord2.senderPublicKey.address) shouldBe empty
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
    oh.allOrderIdsByAddress(ord1.senderPublicKey.address) shouldBe Set(ord1.idStr())
    oh.activeOrderIdsByAddress(ord1.senderPublicKey.address) shouldBe empty

    oh.openVolume(AssetAcc(ord2.senderPublicKey, pair.amountAsset)) shouldBe
      math.max(0L, OrderInfo.safeSum(ord2.matcherFee * 2 / 12, -20000000L))
    oh.openVolume(AssetAcc(ord2.senderPublicKey, pair.priceAsset)) shouldBe 0.00085 * 20000000L
    oh.allOrderIdsByAddress(ord2.senderPublicKey.address) shouldBe Set(ord2.idStr())
    oh.activeOrderIdsByAddress(ord2.senderPublicKey.address) shouldBe Set(pair.priceAsset -> ord2.idStr())
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
    oh.allOrderIdsByAddress(ord1.senderPublicKey.address) shouldBe Set(ord1.idStr())
    oh.activeOrderIdsByAddress(ord1.senderPublicKey.address) shouldBe empty

    oh.openVolume(AssetAcc(ord2.senderPublicKey, pair.priceAsset)) shouldBe 0L
    oh.openVolume(AssetAcc(ord2.senderPublicKey, pair.amountAsset)) shouldBe 0L
    oh.allOrderIdsByAddress(ord2.senderPublicKey.address) shouldBe Set(ord2.idStr())
    oh.activeOrderIdsByAddress(ord2.senderPublicKey.address) shouldBe empty

    oh.openVolume(AssetAcc(ord3.senderPublicKey, pair.amountAsset)) shouldBe ord3.matcherFee * 3 / 8 + 30000000L
    oh.openVolume(AssetAcc(ord3.senderPublicKey, pair.priceAsset)) shouldBe 0L
    oh.allOrderIdsByAddress(ord3.senderPublicKey.address) shouldBe Set(ord3.idStr())
    oh.activeOrderIdsByAddress(ord3.senderPublicKey.address) shouldBe Set(pair.amountAsset -> ord3.idStr())
  }

  property("Partially with own order") {
    val pk   = PrivateKeyAccount("private".getBytes("utf-8"))
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
    oh.allOrderIdsByAddress(ord1.senderPublicKey.address) shouldBe Set(ord1.idStr(), ord2.idStr())
    oh.activeOrderIdsByAddress(ord1.senderPublicKey.address) shouldBe Set(pair.amountAsset -> ord2.idStr())
  }

  property("Cancel buy order") {
    val ord1 = buy(pair, 0.0008, 100000000, matcherFee = Some(300000L))

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.orderCanceled(OrderCanceled(LimitOrder(ord1)))

    oh.orderStatus(ord1.idStr()) shouldBe LimitOrder.Cancelled(0)

    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe 0L
    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 0L
    oh.allOrderIdsByAddress(ord1.senderPublicKey.address) shouldBe Set(ord1.idStr())
    oh.activeOrderIdsByAddress(ord1.senderPublicKey.address) shouldBe empty
  }

  property("Cancel sell order") {
    val ord1 = sell(pair, 0.0008, 100000000, matcherFee = Some(300000L))

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.orderCanceled(OrderCanceled(LimitOrder(ord1)))

    oh.orderStatus(ord1.idStr()) shouldBe LimitOrder.Cancelled(0)

    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.amountAsset)) shouldBe 0L
    oh.openVolume(AssetAcc(ord1.senderPublicKey, pair.priceAsset)) shouldBe 0L
    oh.allOrderIdsByAddress(ord1.senderPublicKey.address) shouldBe Set(ord1.idStr())
    oh.activeOrderIdsByAddress(ord1.senderPublicKey.address) shouldBe empty
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
    oh.allOrderIdsByAddress(ord1.senderPublicKey.address) shouldBe Set(ord1.idStr())
    oh.activeOrderIdsByAddress(ord1.senderPublicKey.address) shouldBe empty
    oh.allOrderIdsByAddress(ord2.senderPublicKey.address) shouldBe Set(ord2.idStr())
    oh.activeOrderIdsByAddress(ord2.senderPublicKey.address) shouldBe empty
  }

  property("Delete order") {
    val pk   = PrivateKeyAccount("private".getBytes("utf-8"))
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
    oh.allOrderIdsByAddress(ord1.senderPublicKey.address) shouldBe Set(ord1.idStr(), ord2.idStr())

    oh.deleteOrder(ord1.senderPublicKey.address, ord1.idStr()) shouldBe false
    oh.deleteOrder(ord2.senderPublicKey.address, ord2.idStr()) shouldBe true

    oh.allOrderIdsByAddress(ord1.senderPublicKey.address) shouldBe Set(ord1.idStr())
    oh.activeOrderIdsByAddress(ord1.senderPublicKey.address) shouldBe Set(pair.priceAsset -> ord1.idStr())
    oh.activeOrderIdsByAddress(ord2.senderPublicKey.address) shouldBe Set(pair.priceAsset -> ord1.idStr())
  }

  property("Sorting by status then timestamp") {
    val pk   = PrivateKeyAccount("private".getBytes("utf-8"))
    val pair = AssetPair(None, Some(ByteStr("BTC".getBytes)))
    val ord1 = buy(pair, 0.0008, 110000000, Some(pk), Some(300000L), Some(1L)) // Filled
    val ord2 = buy(pair, 0.0006, 120000000, Some(pk), Some(300000L), Some(2L)) // Accepted
    val ord3 = buy(pair, 0.0005, 130000000, Some(pk), Some(300000L), Some(3L)) // Canceled
    val ord4 = sell(pair, 0.00079, 2100000000, Some(pk), Some(300000L), Some(4L)) // Partial
    val ord5 = buy(pair, 0.0004, 130000000, Some(pk), Some(300000L), Some(45)) // Accepted

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.orderAccepted(OrderAdded(LimitOrder(ord2)))
    oh.orderAccepted(OrderAdded(LimitOrder(ord3)))
    oh.orderExecuted(OrderExecuted(LimitOrder(ord4), LimitOrder(ord1)))
    oh.orderAccepted(OrderAdded(LimitOrder.limitOrder(ord4.price, 1000000000, ord4)))
    oh.orderCanceled(OrderCanceled(LimitOrder(ord3)))
    oh.orderAccepted(OrderAdded(LimitOrder(ord5)))

    oh.fetchAllOrderHistory(ord1.senderPublicKey.address).map(_._1) shouldBe
      Seq(ord5.idStr(), ord4.idStr(), ord2.idStr(), ord3.idStr(), ord1.idStr())

    oh.fetchAllActiveOrderHistory(ord1.senderPublicKey.address, internal = false).map(_._1) shouldBe
      Seq(ord5.idStr(), ord4.idStr(), ord2.idStr())
  }

  property("History with more than max limit") {
    val pk     = PrivateKeyAccount("private".getBytes("utf-8"))
    val pair   = AssetPair(None, Some(ByteStr("BTC".getBytes)))
    val orders = mutable.Buffer.empty[Order]
    (0 until matcherSettings.maxOrdersPerRequest).foreach { i =>
      val o = buy(pair, 0.0008 + 0.00001 * i, 100000000, Some(pk), Some(300000L), Some(100L + i))
      orders += o
      oh.orderAccepted(OrderAdded(LimitOrder(o)))
    }

    oh.orderCanceled(OrderCanceled(LimitOrder(orders.last)))
    val newOrder = buy(pair, 0.001, 100000000, Some(pk), Some(300000L), Some(1L))
    oh.orderAccepted(OrderAdded(LimitOrder(newOrder)))
    oh.fetchAllOrderHistory(pk.address).map(_._1) shouldBe orders.reverse.tail.map(_.idStr()) :+ newOrder.idStr()
  }

  property("History with more than max limit and canceled order") {
    val pk     = PrivateKeyAccount("private".getBytes("utf-8"))
    val pair   = AssetPair(None, Some(ByteStr("BTC".getBytes)))
    val orders = mutable.Buffer.empty[Order]
    (0 until matcherSettings.maxOrdersPerRequest + 1).foreach { i =>
      val o = buy(pair, 0.0008 + 0.00001 * i, 100000000, Some(pk), Some(300000L), Some(100L + i))
      orders += o
      oh.orderAccepted(OrderAdded(LimitOrder(o)))
    }

    oh.orderCanceled(OrderCanceled(LimitOrder(orders.last)))
    oh.fetchAllOrderHistory(pk.address).map(_._1) shouldBe orders.reverse.tail.map(_.idStr())
  }

  property("Open Portfolio for two assets") {
    val pk         = PrivateKeyAccount("private".getBytes("utf-8"))
    val ass1       = ByteStr("ASS1".getBytes)
    val ass2       = ByteStr("ASS2".getBytes)
    val pair1      = AssetPair(Some(ass1), None)
    val pair2      = AssetPair(Some(ass2), None)
    val matcherFee = 300000L
    val ord1       = sell(pair1, 0.0008, 10000, Some(pk), Some(matcherFee))
    val ord2       = sell(pair2, 0.0009, 10001, Some(pk), Some(matcherFee))

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.orderAccepted(OrderAdded(LimitOrder(ord2)))

    oh.openPortfolio(pk.address) shouldBe
      OpenPortfolio(
        Map("WAVES"     -> (2 * matcherFee - LimitOrder(ord1).getReceiveAmount - LimitOrder(ord2).getReceiveAmount),
            ass1.base58 -> ord1.amount,
            ass2.base58 -> ord2.amount))
  }

}

package com.wavesplatform.matcher.model

import com.google.common.base.Charsets
import com.wavesplatform.WithDB
import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.matcher.api.DBUtils
import com.wavesplatform.matcher.model.Events.{OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.state.{ByteStr, EitherExt2}
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.account.{Address, PrivateKeyAccount}
import scorex.transaction.AssetId
import scorex.transaction.assets.exchange.{AssetPair, Order}

import scala.collection.mutable

class OrderHistorySpecification
    extends PropSpec
    with WithDB
    with PropertyChecks
    with Matchers
    with MatcherTestData
    with BeforeAndAfterAll
    with BeforeAndAfterEach {

  private def mkAssetId(prefix: String) = {
    val prefixBytes = prefix.getBytes(Charsets.UTF_8)
    Some(ByteStr((prefixBytes ++ Array.fill[Byte](32 - prefixBytes.length)(0.toByte)).take(32)))
  }

  val pair = AssetPair(mkAssetId("WCT"), mkAssetId("BTC"))
  var oh   = new OrderHistory(db, matcherSettings)

  override def beforeEach(): Unit = {
    super.beforeEach()
    oh = new OrderHistory(db, matcherSettings)
  }

  private def activeOrderIds(address: Address, assetIds: Set[Option[AssetId]]): Seq[ByteStr] =
    DBUtils.ordersByAddress(db, address, assetIds, activeOnly = true).map(_._1.id())

  property("New buy order added") {
    val ord1 = buy(pair, 0.0007, 10000)

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.orderInfo(ord1.id()).status shouldBe LimitOrder.Accepted
    oh.orderInfo(ord1.id()) shouldBe OrderInfo(ord1.amount, 0, false)

    oh.openVolume(ord1.senderPublicKey, pair.amountAsset) shouldBe 0L
    oh.openVolume(ord1.senderPublicKey, pair.priceAsset) shouldBe 7L
    oh.openVolume(ord1.senderPublicKey, None) shouldBe ord1.matcherFee

    activeOrderIds(ord1.senderPublicKey, Set(pair.priceAsset)) shouldBe Seq(ord1.id())
  }

  property("New sell order added") {
    val ord1 = sell(pair, 0.0007, 10000)

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.orderInfo(ord1.id()).status shouldBe LimitOrder.Accepted
    oh.openVolume(ord1.senderPublicKey, pair.amountAsset) shouldBe 10000L
    oh.openVolume(ord1.senderPublicKey, pair.priceAsset) shouldBe 0L
    oh.openVolume(ord1.senderPublicKey, None) shouldBe ord1.matcherFee

    activeOrderIds(ord1.senderPublicKey, Set(pair.amountAsset)) shouldBe Seq(ord1.id())
  }

  property("New buy WAVES order added") {
    val pair = AssetPair(None, mkAssetId("BTC"))
    val ord1 = buy(pair, 0.0008, 10000)

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.orderInfo(ord1.id()).status shouldBe LimitOrder.Accepted
    oh.openVolume(ord1.senderPublicKey, pair.amountAsset) shouldBe
      math.max(ord1.matcherFee - ord1.getReceiveAmount(ord1.price, ord1.amount).explicitGet(), 0L)
    oh.openVolume(ord1.senderPublicKey, pair.priceAsset) shouldBe 8L

    activeOrderIds(ord1.senderPublicKey, Set(pair.priceAsset)) shouldBe Seq(ord1.id())
  }

  property("New sell WAVES order added") {
    val pair = AssetPair(None, mkAssetId("BTC"))
    val ord1 = sell(pair, 0.0008, 10000)

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.orderInfo(ord1.id()).status shouldBe LimitOrder.Accepted
    oh.openVolume(ord1.senderPublicKey, pair.amountAsset) shouldBe 10000L + ord1.matcherFee
    oh.openVolume(ord1.senderPublicKey, pair.priceAsset) shouldBe 0L

    activeOrderIds(ord1.senderPublicKey, Set(pair.amountAsset)) shouldBe Seq(ord1.id())
  }

  property("New buy and sell WAVES order added") {
    val pk   = PrivateKeyAccount("private".getBytes("utf-8"))
    val pair = AssetPair(None, mkAssetId("BTC"))
    val ord1 = buy(pair, 0.0008, 100000000, Some(pk))
    val ord2 = sell(pair, 0.0009, 210000000, Some(pk))

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.orderAccepted(OrderAdded(LimitOrder(ord2)))

    oh.orderInfo(ord1.id()).status shouldBe LimitOrder.Accepted
    oh.orderInfo(ord2.id()).status shouldBe LimitOrder.Accepted

    oh.openVolume(ord1.senderPublicKey, pair.amountAsset) shouldBe
      math.max(ord1.matcherFee - ord1.getReceiveAmount(ord1.price, ord1.amount).explicitGet(), 0L) + ord2.amount + ord2.matcherFee
    oh.openVolume(ord1.senderPublicKey, pair.priceAsset) shouldBe
      ord1.getSpendAmount(ord1.price, ord1.amount).explicitGet()

    activeOrderIds(ord1.senderPublicKey, Set(pair.priceAsset)) shouldBe Seq(ord1.id())
    activeOrderIds(ord2.senderPublicKey, Set(pair.amountAsset)) shouldBe Seq(ord2.id())
  }

  property("Buy WAVES order filled") {
    val pair = AssetPair(None, mkAssetId("BTC"))
    val ord1 = buy(pair, 0.0008, 10000)
    val ord2 = sell(pair, 0.0007, 10000)

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.orderExecuted(OrderExecuted(LimitOrder(ord2), LimitOrder(ord1)))

    oh.orderInfo(ord1.id()).status shouldBe LimitOrder.Filled
    oh.orderInfo(ord2.id()).status shouldBe LimitOrder.Filled

    oh.openVolume(ord1.senderPublicKey, pair.amountAsset) shouldBe 0L
    oh.openVolume(ord1.senderPublicKey, pair.priceAsset) shouldBe 0L
    activeOrderIds(ord1.senderPublicKey, Set(None)) shouldBe empty

    oh.openVolume(ord2.senderPublicKey, pair.amountAsset) shouldBe 0L
    oh.openVolume(ord2.senderPublicKey, pair.priceAsset) shouldBe 0L
    activeOrderIds(ord2.senderPublicKey, Set(None)) shouldBe empty
  }

  property("Sell WAVES order - filled, buy order - partial") {
    val pair = AssetPair(None, mkAssetId("BTC"))
    val ord1 = sell(pair, 0.0008, 100000000)
    val ord2 = buy(pair, 0.00085, 120000000)

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    val exec = OrderExecuted(LimitOrder(ord2), LimitOrder(ord1))
    oh.orderExecuted(exec)
    oh.orderAccepted(OrderAdded(exec.submittedRemainingOrder))

    oh.orderInfo(ord1.id()).status shouldBe LimitOrder.Filled
    oh.orderInfo(ord2.id()).status shouldBe LimitOrder.PartiallyFilled(100000000)

    oh.openVolume(ord1.senderPublicKey, pair.amountAsset) shouldBe 0L
    oh.openVolume(ord1.senderPublicKey, pair.priceAsset) shouldBe 0L
    activeOrderIds(ord1.senderPublicKey, Set(None)) shouldBe empty

    oh.openVolume(ord2.senderPublicKey, pair.amountAsset) shouldBe
      math.max(0L, OrderInfo.safeSum(ord2.matcherFee * 2 / 12, -20000000L))
    oh.openVolume(ord2.senderPublicKey, pair.priceAsset) shouldBe 0.00085 * 20000000L
    activeOrderIds(ord2.senderPublicKey, Set(pair.priceAsset)) shouldBe Seq(ord2.id())
  }

  property("Buy WAVES order - filled with 2 steps, sell order - partial") {
    val pair = AssetPair(None, mkAssetId("BTC"))
    val ord1 = buy(pair, 0.0008, 100000000, matcherFee = Some(300001L))
    val ord2 = sell(pair, 0.00075, 50000000, matcherFee = Some(300001L))
    val ord3 = sell(pair, 0.0008, 80000000, matcherFee = Some(300001L))

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    val exec1 = OrderExecuted(LimitOrder(ord2), LimitOrder(ord1))
    oh.orderExecuted(exec1)

    oh.orderInfo(ord1.id()).status shouldBe LimitOrder.PartiallyFilled(50000000)
    oh.orderInfo(ord2.id()).status shouldBe LimitOrder.Filled

    val exec2 = OrderExecuted(LimitOrder(ord3), exec1.counterRemainingOrder)
    oh.orderExecuted(exec2)
    oh.orderAccepted(OrderAdded(exec2.submittedRemainingOrder))

    oh.orderInfo(ord1.id()).status shouldBe LimitOrder.Filled
    oh.orderInfo(ord2.id()).status shouldBe LimitOrder.Filled

    oh.openVolume(ord1.senderPublicKey, pair.priceAsset) shouldBe 0L
    oh.openVolume(ord1.senderPublicKey, pair.amountAsset) shouldBe 0L
    activeOrderIds(ord1.senderPublicKey, Set(None)) shouldBe empty

    oh.openVolume(ord2.senderPublicKey, pair.priceAsset) shouldBe 0L
    oh.openVolume(ord2.senderPublicKey, pair.amountAsset) shouldBe 0L
    activeOrderIds(ord2.senderPublicKey, Set(None)) shouldBe empty

    oh.openVolume(ord3.senderPublicKey, pair.amountAsset) shouldBe ord3.matcherFee * 3 / 8 + 30000000L
    oh.openVolume(ord3.senderPublicKey, pair.priceAsset) shouldBe 0L
    activeOrderIds(ord3.senderPublicKey, Set(pair.amountAsset)) shouldBe Seq(ord3.id())
  }

  property("Partially with own order") {
    val pk   = PrivateKeyAccount("private".getBytes("utf-8"))
    val pair = AssetPair(None, mkAssetId("BTC"))
    val ord1 = buy(pair, 0.0008, 100000000, Some(pk), Some(300000L))
    val ord2 = sell(pair, 0.00079, 210000000, Some(pk), Some(300000L))

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    val exec1 = OrderExecuted(LimitOrder(ord2), LimitOrder(ord1))
    oh.orderExecuted(exec1)
    oh.orderAccepted(OrderAdded(exec1.submittedRemainingOrder))

    oh.orderInfo(ord1.id()).status shouldBe LimitOrder.Filled
    oh.orderInfo(ord2.id()).status shouldBe LimitOrder.PartiallyFilled(100000000)

    oh.openVolume(ord1.senderPublicKey, pair.amountAsset) shouldBe 110000000L + ord2.matcherFee * 11 / 21
    oh.openVolume(ord1.senderPublicKey, pair.priceAsset) shouldBe 0L
    activeOrderIds(ord1.senderPublicKey, Set(pair.amountAsset)) shouldBe Seq(ord2.id())
  }

  property("Cancel buy order") {
    val ord1 = buy(pair, 0.0008, 100000000, matcherFee = Some(300000L))

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.orderCanceled(OrderCanceled(LimitOrder(ord1)))

    oh.orderInfo(ord1.id()).status shouldBe LimitOrder.Cancelled(0)

    oh.openVolume(ord1.senderPublicKey, pair.amountAsset) shouldBe 0L
    oh.openVolume(ord1.senderPublicKey, pair.priceAsset) shouldBe 0L
    activeOrderIds(ord1.senderPublicKey, Set(None)) shouldBe empty
  }

  property("Cancel sell order") {
    val ord1 = sell(pair, 0.0008, 100000000, matcherFee = Some(300000L))

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.orderCanceled(OrderCanceled(LimitOrder(ord1)))

    oh.orderInfo(ord1.id()).status shouldBe LimitOrder.Cancelled(0)

    oh.openVolume(ord1.senderPublicKey, pair.amountAsset) shouldBe 0L
    oh.openVolume(ord1.senderPublicKey, pair.priceAsset) shouldBe 0L
    activeOrderIds(ord1.senderPublicKey, Set(None)) shouldBe empty
  }

  property("Cancel partially executed order") {
    val pair = AssetPair(None, mkAssetId("BTC"))
    val ord1 = sell(pair, 0.0008, 2100000000, matcherFee = Some(300000L))
    val ord2 = buy(pair, 0.00081, 1000000000, matcherFee = Some(300000L))

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    val exec1 = OrderExecuted(LimitOrder(ord2), LimitOrder(ord1))
    oh.orderExecuted(exec1)
    oh.orderCanceled(OrderCanceled(exec1.counterRemainingOrder))

    oh.orderInfo(ord1.id()).status shouldBe LimitOrder.Cancelled(1000000000)
    oh.orderInfo(ord2.id()).status shouldBe LimitOrder.Filled

    oh.openVolume(ord1.senderPublicKey, pair.amountAsset) shouldBe 0L
    oh.openVolume(ord1.senderPublicKey, pair.priceAsset) shouldBe 0L
    activeOrderIds(ord1.senderPublicKey, Set(None)) shouldBe empty
    activeOrderIds(ord2.senderPublicKey, Set(None)) shouldBe empty
  }

  property("Delete order") {
    val pk   = PrivateKeyAccount("private".getBytes("utf-8"))
    val pair = AssetPair(None, mkAssetId("BTC"))
    val ord1 = buy(pair, 0.0008, 210000000, Some(pk), Some(300000L))
    val ord2 = sell(pair, 0.00079, 100000000, Some(pk), Some(300000L))

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    val exec1 = OrderExecuted(LimitOrder(ord2), LimitOrder(ord1))
    oh.orderExecuted(exec1)

    oh.orderInfo(ord1.id()).status shouldBe LimitOrder.PartiallyFilled(100000000)
    oh.orderInfo(ord2.id()).status shouldBe LimitOrder.Filled

    oh.openVolume(ord1.senderPublicKey, pair.amountAsset) shouldBe 0L
    oh.openVolume(ord1.senderPublicKey, pair.priceAsset) shouldBe 0.0008 * 110000000L

    oh.deleteOrder(ord1.senderPublicKey, ord1.id()) shouldBe false
    oh.deleteOrder(ord2.senderPublicKey, ord2.id()) shouldBe true

    activeOrderIds(ord1.senderPublicKey, Set(pair.priceAsset)) shouldBe Seq(ord1.id())
    activeOrderIds(ord2.senderPublicKey, Set(pair.priceAsset)) shouldBe Seq(ord1.id())
  }

  property("Sorting by status then timestamp") {
    val pk   = PrivateKeyAccount("private".getBytes("utf-8"))
    val pair = AssetPair(None, mkAssetId("BTC"))
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

    // todo:
//    oh.fetchAllOrderHistory(ord1.senderPublicKey.address).map(_._1) shouldBe
//      Seq(ord5.idStr(), ord4.idStr(), ord2.idStr(), ord3.idStr(), ord1.idStr())
//
//    oh.fetchAllActiveOrderHistory(ord1.senderPublicKey.address).map(_._1) shouldBe
//      Seq(ord5.idStr(), ord4.idStr(), ord2.idStr())
  }

  property("History with more than max limit") {
    val pk     = PrivateKeyAccount("private".getBytes("utf-8"))
    val pair   = AssetPair(None, mkAssetId("BTC"))
    val orders = mutable.Buffer.empty[Order]
    (0 until matcherSettings.maxOrdersPerRequest).foreach { i =>
      val o = buy(pair, 0.0008 + 0.00001 * i, 100000000, Some(pk), Some(300000L), Some(100L + i))
      orders += o
      oh.orderAccepted(OrderAdded(LimitOrder(o)))
    }

    // todo:
//    oh.orderCanceled(OrderCanceled(LimitOrder(orders.last)))
//    val newOrder = buy(pair, 0.001, 100000000, Some(pk), Some(300000L), Some(1L))
//    oh.orderAccepted(OrderAdded(LimitOrder(newOrder)))
//    oh.fetchAllOrderHistory(pk.address).map(_._1) shouldBe orders.reverse.tail.map(_.idStr()) :+ newOrder.idStr()
  }

  property("History with more than max limit and canceled order") {
    val pk     = PrivateKeyAccount("private".getBytes("utf-8"))
    val pair   = AssetPair(None, mkAssetId("BTC"))
    val orders = mutable.Buffer.empty[Order]
    (0 until matcherSettings.maxOrdersPerRequest + 1).foreach { i =>
      val o = buy(pair, 0.0008 + 0.00001 * i, 100000000, Some(pk), Some(300000L), Some(100L + i))
      orders += o
      oh.orderAccepted(OrderAdded(LimitOrder(o)))
    }

    oh.orderCanceled(OrderCanceled(LimitOrder(orders.last)))
    // todo:
//    oh.fetchAllOrderHistory(pk.address).map(_._1) shouldBe orders.reverse.tail.map(_.idStr())
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

    // todo:
//    oh.openPortfolio(pk.address) shouldBe
//      OpenPortfolio(
//        Map("WAVES"     -> (2 * matcherFee - LimitOrder(ord1).getReceiveAmount - LimitOrder(ord2).getReceiveAmount),
//            ass1.base58 -> ord1.amount,
//            ass2.base58 -> ord2.amount))
  }
}

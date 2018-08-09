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
    DBUtils.ordersByAddress(db, address, assetIds, activeOnly = true, matcherSettings.maxOrdersPerRequest).map(_._1.id())

  private def allOrderIds(address: Address, assetIds: Set[Option[AssetId]]): Seq[ByteStr] =
    DBUtils.ordersByAddress(db, address, assetIds, activeOnly = false, matcherSettings.maxOrdersPerRequest).map(_._1.id())

  property("New buy order added") {
    val ord1 = buy(pair, 0.0007, 10000)

    val lo = LimitOrder(ord1)
    oh.orderAccepted(OrderAdded(lo))
    oh.orderInfo(ord1.id()).status shouldBe LimitOrder.Accepted
    oh.orderInfo(ord1.id()) shouldBe OrderInfo(ord1.amount, 0, canceled = false, Some(lo.minAmountOfAmountAsset), ord1.matcherFee)

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
    val ord  = buy(pair, 0.0008, 10000, matcherFee = Some(1000))
    val lo   = LimitOrder(ord)

    oh.orderAccepted(OrderAdded(lo))
    oh.orderInfo(ord.id()).status shouldBe LimitOrder.Accepted
    oh.openVolume(ord.senderPublicKey, pair.amountAsset) shouldBe
      math.max(ord.matcherFee - ord.getReceiveAmount(ord.price, ord.amount).explicitGet(), 0L)
    oh.openVolume(ord.senderPublicKey, pair.priceAsset) shouldBe 8L

    activeOrderIds(ord.senderPublicKey, Set(pair.priceAsset)) shouldBe Seq(ord.id())
  }

  property("New sell WAVES order added") {
    val pair = AssetPair(None, mkAssetId("BTC"))
    val ord  = sell(pair, 0.0008, 10000)
    val lo   = LimitOrder(ord)

    oh.orderAccepted(OrderAdded(lo))
    oh.orderInfo(ord.id()).status shouldBe LimitOrder.Accepted
    oh.openVolume(ord.senderPublicKey, pair.amountAsset) shouldBe 10000L + ord.matcherFee
    oh.openVolume(ord.senderPublicKey, pair.priceAsset) shouldBe 0L

    activeOrderIds(ord.senderPublicKey, Set(pair.amountAsset)) shouldBe Seq(ord.id())
  }

  property("New buy and sell WAVES order added") {
    val pk   = PrivateKeyAccount("private".getBytes("utf-8"))
    val pair = AssetPair(None, mkAssetId("BTC"))
    val ord1 = buy(pair, 0.0008, 100000000, Some(pk), matcherFee = Some(1000L))
    val ord2 = sell(pair, 0.0009, 210000000, Some(pk), matcherFee = Some(1000L))

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.orderAccepted(OrderAdded(LimitOrder(ord2)))

    oh.orderInfo(ord1.id()).status shouldBe LimitOrder.Accepted
    oh.orderInfo(ord2.id()).status shouldBe LimitOrder.Accepted

    oh.openVolume(ord1.senderPublicKey, pair.amountAsset) shouldBe
      math.max(ord1.matcherFee - ord1.getReceiveAmount(ord1.price, ord1.amount).explicitGet(), 0L) + Order.correctAmount(ord2.amount, ord2.price) + ord2.matcherFee
    oh.openVolume(ord1.senderPublicKey, pair.priceAsset) shouldBe
      ord1.getSpendAmount(ord1.price, ord1.amount).explicitGet()

    activeOrderIds(ord1.senderPublicKey, Set(pair.priceAsset)) shouldBe Seq(ord1.id())
    activeOrderIds(ord2.senderPublicKey, Set(pair.amountAsset)) shouldBe Seq(ord2.id())
  }

  property("Buy WAVES order filled") {
    val pair = AssetPair(None, mkAssetId("BTC"))
    val ord1 = buy(pair, 0.0008, 10000, matcherFee = Some(2000L))
    val ord2 = sell(pair, 0.0007, 10000, matcherFee = Some(1000L))

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    val exec = OrderExecuted(LimitOrder(ord2), LimitOrder(ord1))
    oh.orderExecuted(exec)

    exec shouldBe 8574
    oh.orderInfo(ord1.id()).status shouldBe LimitOrder.Filled(exec.executedAmount)
    oh.orderInfo(ord2.id()).status shouldBe LimitOrder.Filled(exec.executedAmount)

    oh.openVolume(ord1.senderPublicKey, pair.amountAsset) shouldBe 0L
    oh.openVolume(ord1.senderPublicKey, pair.priceAsset) shouldBe 0L
    activeOrderIds(ord1.senderPublicKey, Set(None)) shouldBe empty

    oh.openVolume(ord2.senderPublicKey, pair.amountAsset) shouldBe 0L
    oh.openVolume(ord2.senderPublicKey, pair.priceAsset) shouldBe 0L
    activeOrderIds(ord2.senderPublicKey, Set(None)) shouldBe empty
  }

  property("Sell WAVES order - filled, buy order - partial") {
    val pair      = AssetPair(None, mkAssetId("BTC"))
    val counter   = sell(pair, 0.0008, 100000000, matcherFee = Some(2000L))
    val submitted = buy(pair, 0.00085, 120000000, matcherFee = Some(1000L))

    println("\norderAccepted start\n")
    // check info
    oh.orderAccepted(OrderAdded(LimitOrder(counter)))
    println("\norderAccepted end\n")

    // check info
    val exec = OrderExecuted(LimitOrder(submitted), LimitOrder(counter))

    println(s"""|
                |submitted:
                |will receive: ${exec.submitted.getReceiveAmount} ${exec.submitted.rcvAsset}
                |will spend: ${exec.submitted.getSpendAmount} ${exec.submitted.spentAsset}
                |""".stripMargin)
    println("\norderExecuted start\n")
    oh.orderExecuted(exec)
    println("\norderExecuted end\n")

    val counterOrderInfo = oh.orderInfo(counter.id())
    withClue(s"counter: ${counter.id()}") {
      exec.counterRemainingAmount shouldBe 0L
      exec.counterRemainingAmount shouldBe counterOrderInfo.remaining

      exec.counterRemainingFee shouldBe 0L
      exec.counterRemainingFee shouldBe counterOrderInfo.remainingFee

      counterOrderInfo.status shouldBe LimitOrder.Filled(100000000)
    }

    val submittedOrderInfo = oh.orderInfo(submitted.id())
    withClue(s"submitted: ${submitted.id()}") {
      exec.submittedRemainingAmount shouldBe 20000000L
      exec.submittedRemainingAmount shouldBe submittedOrderInfo.remaining

      exec.submittedRemainingFee shouldBe 167L
      exec.submittedRemainingFee shouldBe submittedOrderInfo.remainingFee

      submittedOrderInfo.status shouldBe LimitOrder.PartiallyFilled(100000000)
    }

    // ord1 == counter
    // ord2 == submitted
    oh.openVolume(submitted.senderPublicKey, pair.amountAsset) shouldBe
      math.max(0L, OrderInfo.safeSum(LimitOrder.getPartialFee(submitted.matcherFee, submitted.amount, submitted.amount - counter.amount), -20000000L))
    oh.openVolume(submitted.senderPublicKey, pair.priceAsset) shouldBe (BigDecimal(0.00085) * 20000000L).toLong
    activeOrderIds(submitted.senderPublicKey, Set(pair.priceAsset)) shouldBe Seq(submitted.id())
  }

  property("Buy WAVES order - filled with 2 steps, sell order - partial") {
    val pair = AssetPair(None, mkAssetId("BTC"))
    val ord1 = buy(pair, 0.0008, 100000000, matcherFee = Some(300001L))
    val ord2 = sell(pair, 0.00075, 50000000, matcherFee = Some(300001L))
    val ord3 = sell(pair, 0.0008, 80000000, matcherFee = Some(300001L))

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    println(s"== order added ord1: ${ord1.id()}")
    val exec1 = OrderExecuted(LimitOrder(ord2), LimitOrder(ord1)) //LimitOrder.limitOrder(ord1.price, ord1.amount - ord2.amount, 150001L, ord1))
    println(s"== order executed: submitted=ord2 (${ord2.id()}), counter=ord1 (${ord1.id()}")
    oh.orderExecuted(exec1)

    println(s"""
         |orderInfo:
         |ord1 (id=${ord1.id()}, status=${oh.orderInfo(ord1.id()).status}): ${oh.orderInfo(ord1.id())}
         |ord2 (id=${ord2.id()}, status=${oh.orderInfo(ord2.id()).status}): ${oh.orderInfo(ord2.id())}
         |ord3 (id=${ord3.id()}, status=${oh.orderInfo(ord3.id()).status}): ${oh.orderInfo(ord3.id())}
         |""".stripMargin)

    val o1Info1 = oh.orderInfo(ord1.id())
    val o2Info1 = oh.orderInfo(ord2.id())

    // ord3 & ord1
    exec1.counterRemainingAmount shouldBe o1Info1.remaining
    exec1.counterRemainingFee shouldBe o1Info1.remainingFee
    o1Info1.status shouldBe LimitOrder.PartiallyFilled(50000000)

    exec1.submittedRemainingAmount shouldBe o2Info1.remaining
    exec1.submittedRemainingFee shouldBe o2Info1.remainingFee
    o2Info1.status shouldBe LimitOrder.Filled(50000000)

    oh.orderInfo(ord3.id()).status shouldBe LimitOrder.NotFound

    val exec2 = OrderExecuted(LimitOrder(ord3), exec1.counterRemaining) // ord1
    println(s"""== order executed: ${ord3.id()}, ${exec1.counter.order.id()}
               |exec2.counterRemaining (ord1: ${ord1.id()}): ${exec2.counterRemaining}
               |exec2.submittedRemaining (ord3: ${ord3.id()}): ${exec2.submittedRemaining}
               |""".stripMargin)
    oh.orderExecuted(exec2)
//    oh.orderAccepted(OrderAdded(exec2.submitted.partial(exec2.submittedRemainingAmount, exec1.submittedRemainingFee)))
//    println(s"==    order added: ${exec2.submitted.order.id()}")

    println(s"""
         |orderInfo:
         |ord1 (id=${ord1.id()}, status=${oh.orderInfo(ord1.id()).status}): ${oh.orderInfo(ord1.id())}
         |ord2 (id=${ord2.id()}, status=${oh.orderInfo(ord2.id()).status}): ${oh.orderInfo(ord2.id())}
         |ord3 (id=${ord3.id()}, status=${oh.orderInfo(ord3.id()).status}): ${oh.orderInfo(ord3.id())}
         |""".stripMargin)

    val o1Info2 = oh.orderInfo(ord1.id())
    exec2.counterRemainingAmount shouldBe o1Info2.remaining
    //exec2.counterRemainingFee shouldBe o1Info2.remainingFee // 1 vs 0
    oh.orderInfo(ord1.id()).status shouldBe LimitOrder.Filled(100000000) // fails

    oh.orderInfo(ord2.id()).status shouldBe LimitOrder.Filled(50000000)

    val o3Info2 = oh.orderInfo(ord3.id())
    exec2.submittedRemainingAmount shouldBe o3Info2.remaining
    exec2.submittedRemainingFee shouldBe o3Info2.remainingFee
    oh.orderInfo(ord3.id()).status shouldBe LimitOrder.PartiallyFilled(50000000)

    oh.openVolume(ord1.senderPublicKey, pair.priceAsset) shouldBe 0L
    oh.openVolume(ord1.senderPublicKey, pair.amountAsset) shouldBe 0L
    activeOrderIds(ord1.senderPublicKey, Set(None)) shouldBe empty

    oh.openVolume(ord2.senderPublicKey, pair.priceAsset) shouldBe 0L
    oh.openVolume(ord2.senderPublicKey, pair.amountAsset) shouldBe 0L
    activeOrderIds(ord2.senderPublicKey, Set(None)) shouldBe empty

    println(s"""ord3:
         |amount: ${o3Info2.amount - o3Info2.filled + o3Info2.remainingFee}
       """.stripMargin)
    oh.openVolume(ord3.senderPublicKey, pair.amountAsset) shouldBe (o3Info2.amount - o3Info2.filled + o3Info2.remainingFee) // ord3.matcherFee * 3 / 8 + 30000000L
    oh.openVolume(ord3.senderPublicKey, pair.priceAsset) shouldBe 0L
    activeOrderIds(ord3.senderPublicKey, Set(pair.amountAsset)) shouldBe Seq(ord3.id())
  }

  property("Partially with own order") {
    val pk        = PrivateKeyAccount("private".getBytes("utf-8"))
    val pair      = AssetPair(None, mkAssetId("BTC"))
    val counter   = buy(pair, 0.0008, 100000000, Some(pk), Some(300000L))
    val submitted = sell(pair, 0.00079, 210000000, Some(pk), Some(300000L))

    println(s"""|
          |before:
          |oh.openVolume(${counter.senderPublicKey}, ${pair.amountAsset}): ${oh.openVolume(counter.senderPublicKey, pair.amountAsset)}
          |""".stripMargin)
    oh.orderAccepted(OrderAdded(LimitOrder(counter)))
    println(s"""|
          |after accept:
          |oh.openVolume(${counter.senderPublicKey}, ${pair.amountAsset}): ${oh.openVolume(counter.senderPublicKey, pair.amountAsset)}
          |""".stripMargin)

    val exec = OrderExecuted(LimitOrder(submitted), LimitOrder(counter))
    println(s"""|
                |amount: ${exec.executedAmount}
                |
                |counter:
                |will receive: ${exec.counter.getReceiveAmount} ${exec.counter.rcvAsset}
                |will spend: ${exec.counter.getSpendAmount} ${exec.counter.spentAsset}
                |""".stripMargin)

    println(s"""|
                |submitted:
                |will receive: ${exec.submitted.getReceiveAmount} ${exec.submitted.rcvAsset}
                |will spend: ${exec.submitted.getSpendAmount} ${exec.submitted.spentAsset}
                |""".stripMargin)

    oh.orderExecuted(exec)

    println(s"""|
          |after execute:
          |oh.openVolume(${counter.senderPublicKey}, ${pair.amountAsset}): ${oh.openVolume(counter.senderPublicKey, pair.amountAsset)}
          |""".stripMargin)

    val counterOrderInfo = oh.orderInfo(counter.id())
    withClue(s"counter: ${counter.id()}") {
      exec.counterRemainingAmount shouldBe 0L
      exec.counterRemainingAmount shouldBe counterOrderInfo.remaining

      exec.counterRemainingFee shouldBe 0L
      exec.counterRemainingFee shouldBe counterOrderInfo.remainingFee

      counterOrderInfo.status shouldBe LimitOrder.Filled(100000000)
    }

    val submittedOrderInfo = oh.orderInfo(submitted.id())
//    withClue(s"submitted: ${submitted.id()}") {
//      exec.submittedRemainingAmount shouldBe 110000000L
//      exec.submittedRemainingAmount shouldBe submittedOrderInfo.remaining
//
//      exec.submittedRemainingFee shouldBe 157143L
//      exec.submittedRemainingFee shouldBe submittedOrderInfo.remainingFee

    /*
      withClue(s"account checks, counter.senderPublicKey: ${counter.senderPublicKey}, counter.order.id=${counter.id()}") {
      // Will receive 165900 < fee
      // 208!
      println(s"""|
            |exec.executedAmount: ${exec.executedAmount}
            |
            |exec.submittedRemainingAmount: ${exec.submittedRemainingAmount}
            |exec.submittedRemainingFee: ${exec.submittedRemainingFee}
            |
            |exec.counterRemainingAmount: ${exec.counterRemainingAmount}
            |exec.counterRemainingFee: ${exec.counterRemainingFee}
            |
            |exec.counter.amountOfAmountAsset: ${exec.counter.amountOfAmountAsset}
            |exec.counter.minAmountOfAmountAsset: ${exec.counter.minAmountOfAmountAsset}
            |exec.submitted.amountOfAmountAsset: ${exec.submitted.amountOfAmountAsset}
            |exec.submitted.minAmountOfAmountAsset: ${exec.submitted.minAmountOfAmountAsset}
            |""".stripMargin)
      oh.openVolume(pk, pair.amountAsset) shouldBe exec.submitted.amountOfAmountAsset - exec.executedAmount + LimitOrder
        .getPartialFee(
          submitted.matcherFee,
          submitted.amount,
          submitted.amount - exec.executedAmount
        ) + exec.counter.amountOfAmountAsset - exec.executedAmount + LimitOrder
        .getPartialFee(
          counter.matcherFee,
          counter.amount,
          counter.amount - exec.executedAmount
        )
      oh.openVolume(counter.senderPublicKey, pair.priceAsset) shouldBe 0L
      activeOrderIds(counter.senderPublicKey, Set(pair.amountAsset)) shouldBe Seq(submitted.id())
    }
     */

    oh.openVolume(counter.senderPublicKey, pair.amountAsset) shouldBe 110000000L + LimitOrder.getPartialFee(
      submitted.matcherFee,
      submitted.amount,
      submitted.amount - counter.amount
    )
    oh.openVolume(counter.senderPublicKey, pair.priceAsset) shouldBe 0L
    activeOrderIds(counter.senderPublicKey, Set(pair.amountAsset)) shouldBe Seq(submitted.id())
  }

  property("Cancel buy order") {
    val ord1 = buy(pair, 0.0008, 100000000, matcherFee = Some(300000L))

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.orderCanceled(OrderCanceled(LimitOrder(ord1), unmatchable = false))

    oh.orderInfo(ord1.id()).status shouldBe LimitOrder.Cancelled(0)

    oh.openVolume(ord1.senderPublicKey, pair.amountAsset) shouldBe 0L
    oh.openVolume(ord1.senderPublicKey, pair.priceAsset) shouldBe 0L
    activeOrderIds(ord1.senderPublicKey, Set(None)) shouldBe empty
  }

  property("Cancel sell order") {
    val ord1 = sell(pair, 0.0008, 100000000, matcherFee = Some(300000L))

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.orderCanceled(OrderCanceled(LimitOrder(ord1), unmatchable = false))

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
    oh.orderCanceled(OrderCanceled(exec1.counter.partial(exec1.counterRemainingAmount, exec1.counterRemainingFee), unmatchable = false))

    oh.orderInfo(ord1.id()).status shouldBe LimitOrder.Cancelled(1000000000)
    oh.orderInfo(ord2.id()).status shouldBe LimitOrder.Filled(1000000000)

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
    oh.orderInfo(ord2.id()).status shouldBe LimitOrder.Filled(100000000)

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
    oh.orderAccepted(OrderAdded(LimitOrder.limitOrder(ord4.price, 1000000000, 0, ord4)))
    oh.orderCanceled(OrderCanceled(LimitOrder(ord3), unmatchable = false))
    oh.orderAccepted(OrderAdded(LimitOrder(ord5)))

    allOrderIds(ord1.senderPublicKey, Set.empty) shouldBe
      Seq(ord5.id(), ord4.id(), ord2.id(), ord3.id(), ord1.id())

    activeOrderIds(ord1.senderPublicKey, Set.empty) shouldBe
      Seq(ord5.id(), ord4.id(), ord2.id())
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

    oh.orderCanceled(OrderCanceled(LimitOrder(orders.last), unmatchable = false))

    val newOrder = buy(pair, 0.001, 100000000, Some(pk), Some(300000L), Some(1L))

    oh.orderAccepted(OrderAdded(LimitOrder(newOrder)))

    allOrderIds(pk, Set.empty) shouldBe orders.reverse.tail.map(_.id()) :+ newOrder.id()
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

    oh.orderCanceled(OrderCanceled(LimitOrder(orders.last), unmatchable = false))
    allOrderIds(pk, Set.empty) shouldBe orders.reverse.tail.map(_.id())
  }

  property("Open Portfolio for two assets") {
    val pk         = PrivateKeyAccount("private".getBytes("utf-8"))
    val ass1       = mkAssetId("ASS1")
    val ass2       = mkAssetId("ASS2")
    val pair1      = AssetPair(ass1, None)
    val pair2      = AssetPair(ass2, None)
    val matcherFee = 300000L
    val ord1       = sell(pair1, 0.0008, 10000, Some(pk), Some(matcherFee))
    val ord2       = sell(pair2, 0.0009, 10001, Some(pk), Some(matcherFee))

    oh.orderAccepted(OrderAdded(LimitOrder(ord1)))
    oh.orderAccepted(OrderAdded(LimitOrder(ord2)))

    DBUtils.reservedBalance(db, pk) shouldBe
      Map(
        None -> (2 * matcherFee - LimitOrder(ord1).getReceiveAmount - LimitOrder(ord2).getReceiveAmount),
        ass1 -> Order.correctAmount(ord1.amount, ord1.price),
        ass2 -> Order.correctAmount(ord2.amount, ord2.price)
      )
  }
}

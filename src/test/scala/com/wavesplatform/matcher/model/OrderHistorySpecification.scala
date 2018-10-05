package com.wavesplatform.matcher.model

import com.google.common.base.Charsets
import com.wavesplatform.WithDB
import com.wavesplatform.account.{Address, PrivateKeyAccount}
import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.matcher.api.DBUtils
import com.wavesplatform.matcher.model.Events.{OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.matcher.model.OrderHistorySpecification._
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.assets.exchange.AssetPair
import org.scalatest._
import org.scalatest.prop.PropertyChecks

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

  private def activeOrderIds(address: Address): Seq[ByteStr] =
    DBUtils.ordersByAddress(db, address, activeOnly = true, matcherSettings.maxOrdersPerRequest).map(_._1.id())

  private def allOrderIds(address: Address): Seq[ByteStr] =
    DBUtils.ordersByAddress(db, address, activeOnly = false, matcherSettings.maxOrdersPerRequest).map(_._1.id())

  private def activeOrderIdsByPair(address: Address, pair: AssetPair): Seq[ByteStr] =
    DBUtils.ordersByAddressAndPair(db, address, pair, matcherSettings.maxOrdersPerRequest).collect {
      case (o, s) if !s.status.isFinal => o.id()
    }

  private def allOrderIdsByPair(address: Address, pair: AssetPair): Seq[ByteStr] =
    DBUtils.ordersByAddressAndPair(db, address, pair, matcherSettings.maxOrdersPerRequest).map(_._1.id())

  property("New buy order added") {
    val ord = buy(pair, 0.0007, 10000)

    val lo = LimitOrder(ord)
    oh.process(OrderAdded(lo))

    val info = oh.orderInfo(ord.id())
    withClue("info") {
      info.status shouldBe LimitOrder.Accepted
      info shouldBe OrderInfo(ord.amount, 0, None, Some(lo.minAmountOfAmountAsset), ord.matcherFee, Some(0L))
    }

    withClue("reserved assets") {
      oh.openVolume(ord.senderPublicKey, pair.amountAsset) shouldBe 0L
      oh.openVolume(ord.senderPublicKey, pair.priceAsset) shouldBe 7L
      oh.openVolume(ord.senderPublicKey, None) shouldBe ord.matcherFee
    }

    withClue("orders list") {
      val expected = Seq(ord.id())

      activeOrderIds(ord.senderPublicKey) shouldBe expected
      allOrderIds(ord.senderPublicKey) shouldBe expected

      activeOrderIdsByPair(ord.senderPublicKey, pair) shouldBe expected
      allOrderIdsByPair(ord.senderPublicKey, pair) shouldBe expected
    }
  }

  property("New sell order added") {
    val ord = sell(pair, 0.0007, 10000)

    val lo = LimitOrder(ord)
    oh.process(OrderAdded(LimitOrder(ord)))

    val info = oh.orderInfo(ord.id())
    withClue("info") {
      info.status shouldBe LimitOrder.Accepted
      info shouldBe OrderInfo(ord.amount, 0, None, Some(lo.minAmountOfAmountAsset), ord.matcherFee, Some(0L))
    }

    withClue("reserved assets") {
      oh.openVolume(ord.senderPublicKey, pair.amountAsset) shouldBe 10000L
      oh.openVolume(ord.senderPublicKey, pair.priceAsset) shouldBe 0L
      oh.openVolume(ord.senderPublicKey, None) shouldBe ord.matcherFee
    }

    withClue("orders list") {
      val expected = Seq(ord.id())

      activeOrderIds(ord.senderPublicKey) shouldBe expected
      allOrderIds(ord.senderPublicKey) shouldBe expected

      activeOrderIdsByPair(ord.senderPublicKey, pair) shouldBe expected
      allOrderIdsByPair(ord.senderPublicKey, pair) shouldBe expected
    }
  }

  property("New buy WAVES order added") {
    val pair = AssetPair(None, mkAssetId("BTC"))
    val ord  = buy(pair, 0.008, 1000, matcherFee = Some(3000))
    val lo   = LimitOrder(ord)

    oh.process(OrderAdded(lo))

    val info = oh.orderInfo(ord.id())
    withClue("info") {
      info.status shouldBe LimitOrder.Accepted
      info shouldBe OrderInfo(ord.amount, 0, None, Some(lo.minAmountOfAmountAsset), ord.matcherFee, Some(0L))
    }

    withClue("reserved assets considering amount of received WAVES") {
      oh.openVolume(ord.senderPublicKey, pair.amountAsset) shouldBe 2000L
      oh.openVolume(ord.senderPublicKey, pair.priceAsset) shouldBe 8L
    }

    activeOrderIds(ord.senderPublicKey) shouldBe Seq(ord.id())
  }

  property("New sell WAVES order added") {
    val pair = AssetPair(None, mkAssetId("BTC"))
    val ord  = sell(pair, 0.0008, 10000)
    val lo   = LimitOrder(ord)

    oh.process(OrderAdded(lo))
    oh.orderInfo(ord.id()).status shouldBe LimitOrder.Accepted
    oh.openVolume(ord.senderPublicKey, pair.amountAsset) shouldBe 10000L + ord.matcherFee
    oh.openVolume(ord.senderPublicKey, pair.priceAsset) shouldBe 0L

    activeOrderIds(ord.senderPublicKey) shouldBe Seq(ord.id())
  }

  property("Should not reserve fee, if seller receives more WAVES than total fee in sell order") {
    val pair = AssetPair(mkAssetId("BTC"), None)
    val ord  = sell(pair, 0.01, 100000, matcherFee = Some(1000L))

    oh.process(OrderAdded(LimitOrder(ord)))

    val oi = oh.orderInfo(ord.id())
    oi.status shouldBe LimitOrder.Accepted

    oh.openVolume(ord.senderPublicKey, pair.priceAsset) shouldBe 0L
  }

  property("Should not reserve fee, if buyer receives more WAVES than total fee in buy order") {
    val pair = AssetPair(None, mkAssetId("BTC"))
    val ord  = buy(pair, 0.0007, 100000, matcherFee = Some(1000L))

    oh.process(OrderAdded(LimitOrder(ord)))

    val oi = oh.orderInfo(ord.id())
    oi.status shouldBe LimitOrder.Accepted

    oh.openVolume(ord.senderPublicKey, pair.amountAsset) shouldBe 0L
  }

  property("Two sell orders added") {
    val pk   = PrivateKeyAccount("private".getBytes("utf-8"))
    val pair = AssetPair(None, mkAssetId("BTC"))
    val ord1 = sell(pair, 0.0005, 10000, Some(pk), matcherFee = Some(30000L), ts = Some(System.currentTimeMillis()))
    val ord2 = sell(pair, 0.0008, 16000, Some(pk), matcherFee = Some(30000L), ts = Some(System.currentTimeMillis() + 1))

    oh.processAll(OrderAdded(LimitOrder(ord1)), OrderAdded(LimitOrder(ord2)))

    withClue("all orders accepted") {
      oh.orderInfo(ord1.id()).status shouldBe LimitOrder.Accepted
      oh.orderInfo(ord2.id()).status shouldBe LimitOrder.Accepted
    }

    withClue("correction was used to reserve assets") {
      oh.openVolume(ord1.senderPublicKey, pair.amountAsset) shouldBe ord1.amount + ord1.matcherFee + ord2.amount + ord2.matcherFee
      oh.openVolume(ord1.senderPublicKey, pair.priceAsset) shouldBe 0L
    }

    withClue("orders list") {
      val expected = Seq(ord2.id(), ord1.id())

      activeOrderIds(ord1.senderPublicKey) shouldBe expected
      allOrderIds(ord1.senderPublicKey) shouldBe expected

      activeOrderIdsByPair(ord1.senderPublicKey, pair) shouldBe expected
      allOrderIdsByPair(ord1.senderPublicKey, pair) shouldBe expected
    }
  }

  property("Buy WAVES order filled exactly") {
    val pair      = AssetPair(None, mkAssetId("BTC"))
    val counter   = buy(pair, 0.0008, 100000, matcherFee = Some(2000L))
    val submitted = sell(pair, 0.0007, 100000, matcherFee = Some(1000L))

    oh.process(OrderAdded(LimitOrder(counter)))

    val exec = OrderExecuted(LimitOrder(submitted), LimitOrder(counter))
    oh.process(exec)

    withClue("executed exactly") {
      exec.executedAmount shouldBe counter.amount
      oh.orderInfo(counter.id()).status shouldBe LimitOrder.Filled(exec.executedAmount)
      oh.orderInfo(submitted.id()).status shouldBe LimitOrder.Filled(exec.executedAmount)
    }

    withClue(s"has no reserved assets, counter.senderPublicKey: ${counter.senderPublicKey}, counter.order.id=${counter.idStr()}") {
      oh.openVolume(counter.senderPublicKey, pair.amountAsset) shouldBe 0L
      oh.openVolume(counter.senderPublicKey, pair.priceAsset) shouldBe 0L
    }

    withClue(s"has no reserved assets, submitted.senderPublicKey: ${submitted.senderPublicKey}, submitted.order.id=${submitted.idStr()}") {
      oh.openVolume(submitted.senderPublicKey, pair.amountAsset) shouldBe 0L
      oh.openVolume(submitted.senderPublicKey, pair.priceAsset) shouldBe 0L
    }

    withClue("orders list of counter owner") {
      activeOrderIds(counter.senderPublicKey) shouldBe empty
      allOrderIds(counter.senderPublicKey) shouldBe Seq(counter.id())

      activeOrderIdsByPair(counter.senderPublicKey, pair) shouldBe empty
      allOrderIdsByPair(counter.senderPublicKey, pair) shouldBe Seq(counter.id())
    }

    withClue("orders list of submitted owner") {
      activeOrderIds(submitted.senderPublicKey) shouldBe empty
      allOrderIds(submitted.senderPublicKey) shouldBe Seq(submitted.id())

      activeOrderIdsByPair(submitted.senderPublicKey, pair) shouldBe empty
      allOrderIdsByPair(submitted.senderPublicKey, pair) shouldBe Seq(submitted.id())
    }
  }

  property("Buy WAVES order filled with remainder") {
    val pair      = AssetPair(None, mkAssetId("BTC"))
    val counter   = sell(pair, 0.00000238, 840340L, matcherFee = Some(300000L))
    val submitted = buy(pair, 0.00000238, 425532L, matcherFee = Some(300000L))

    val counterLo = LimitOrder(counter)
    oh.process(OrderAdded(counterLo))
    val counterOrderInfo1 = oh.orderInfo(counter.id())
    withClue(s"account checks, counter.senderPublicKey: ${counter.senderPublicKey}, counter.order.id=${counter.idStr()}") {
      oh.openVolume(counter.senderPublicKey, pair.amountAsset) shouldBe counterLo.getRawSpendAmount - counterOrderInfo1.totalSpend(counterLo) + counterOrderInfo1.remainingFee
      oh.openVolume(counter.senderPublicKey, pair.priceAsset) shouldBe 0L
      activeOrderIds(counter.senderPublicKey) shouldBe Seq(counter.id())
    }

    val exec = OrderExecuted(LimitOrder(submitted), LimitOrder(counter))
    exec.executedAmount shouldBe 420169L

    oh.process(exec)
    val counterOrderInfo = oh.orderInfo(counter.id())
    withClue(s"counter.order.id=${submitted.idStr()}") {
      counterOrderInfo.filled shouldBe exec.executedAmount

      exec.counterRemainingAmount shouldBe 420171L
      exec.counterRemainingAmount shouldBe counter.amount - exec.executedAmount
      exec.counterRemainingAmount shouldBe counterOrderInfo.remaining

      exec.counterRemainingFee shouldBe 150001L
      exec.counterRemainingFee shouldBe counterOrderInfo.remainingFee

      counterOrderInfo.status shouldBe LimitOrder.PartiallyFilled(exec.executedAmount)
    }

    val submittedOrderInfo = oh.orderInfo(submitted.id())
    withClue(s"submitted.order.id=${counter.idStr()}") {
      exec.submittedRemainingAmount shouldBe 5363L
      exec.submittedRemainingAmount shouldBe submitted.amount - exec.executedAmount
      exec.submittedRemainingAmount shouldBe submittedOrderInfo.remaining

      exec.submittedRemainingFee shouldBe 3781L
      submittedOrderInfo.status shouldBe LimitOrder.Filled(exec.executedAmount)
    }

    // see OrderBookActor.handleMatchEvent
    oh.process(OrderAdded(exec.submittedRemaining))

    withClue(s"account checks, counter.senderPublicKey: ${counter.senderPublicKey}, counter.order.id=${counter.idStr()}") {
      val remainingSpend = counter.amount - counterOrderInfo.totalSpend(LimitOrder(counter))
      oh.openVolume(counter.senderPublicKey, pair.amountAsset) shouldBe remainingSpend + counterOrderInfo.remainingFee
      oh.openVolume(counter.senderPublicKey, pair.priceAsset) shouldBe 0L
    }

    withClue(s"account checks, submitted.senderPublicKey: ${submitted.senderPublicKey}, submitted.order.id=${submitted.idStr()}") {
      oh.openVolume(submitted.senderPublicKey, pair.amountAsset) shouldBe 0L
      oh.openVolume(submitted.senderPublicKey, pair.priceAsset) shouldBe 0L
    }

    withClue("orders list of counter owner") {
      val expected = Seq(counter.id())

      activeOrderIds(counter.senderPublicKey) shouldBe expected
      allOrderIds(counter.senderPublicKey) shouldBe expected

      activeOrderIdsByPair(counter.senderPublicKey, pair) shouldBe expected
      allOrderIdsByPair(counter.senderPublicKey, pair) shouldBe expected
    }

    withClue("orders list of submitted owner") {
      activeOrderIds(submitted.senderPublicKey) shouldBe empty
      allOrderIds(submitted.senderPublicKey) shouldBe Seq(submitted.id())

      activeOrderIdsByPair(submitted.senderPublicKey, pair) shouldBe empty
      allOrderIdsByPair(submitted.senderPublicKey, pair) shouldBe Seq(submitted.id())
    }
  }

  property("Sell WAVES order - filled, buy order - partial") {
    val pair      = AssetPair(None, mkAssetId("BTC"))
    val counter   = sell(pair, 0.0008, 100000000, matcherFee = Some(2000L))
    val submitted = buy(pair, 0.00085, 120000000, matcherFee = Some(1000L))

    oh.process(OrderAdded(LimitOrder(counter)))
    val exec = OrderExecuted(LimitOrder(submitted), LimitOrder(counter))
    oh.processAll(exec, OrderAdded(exec.submittedRemaining))

    val counterOrderInfo = oh.orderInfo(counter.id())
    withClue(s"counter: ${counter.idStr()}") {
      exec.counterRemainingAmount shouldBe 0L
      exec.counterRemainingAmount shouldBe counterOrderInfo.remaining

      exec.counterRemainingFee shouldBe 0L
      exec.counterRemainingFee shouldBe counterOrderInfo.remainingFee

      counterOrderInfo.status shouldBe LimitOrder.Filled(100000000)
    }

    val submittedOrderInfo = oh.orderInfo(submitted.id())
    withClue(s"submitted: ${submitted.idStr()}") {
      exec.submittedRemainingAmount shouldBe 20000000L
      exec.submittedRemainingAmount shouldBe submittedOrderInfo.remaining

      exec.submittedRemainingFee shouldBe 167L
      exec.submittedRemainingFee shouldBe submittedOrderInfo.remainingFee

      submittedOrderInfo.status shouldBe LimitOrder.PartiallyFilled(100000000)
    }

    withClue(s"account checks, submitted.senderPublicKey: ${submitted.senderPublicKey}, submitted.order.id=${submitted.idStr()}") {
      oh.openVolume(submitted.senderPublicKey, pair.amountAsset) shouldBe
        math.max(0L,
                 OrderInfo.safeSum(LimitOrder.getPartialFee(submitted.matcherFee, submitted.amount, submitted.amount - counter.amount), -20000000L))
      oh.openVolume(submitted.senderPublicKey, pair.priceAsset) shouldBe (BigDecimal(0.00085) * 20000000L).toLong
    }

    withClue("orders list of counter owner") {
      activeOrderIds(counter.senderPublicKey) shouldBe empty
      allOrderIds(counter.senderPublicKey) shouldBe Seq(counter.id())

      activeOrderIdsByPair(counter.senderPublicKey, pair) shouldBe empty
      allOrderIdsByPair(counter.senderPublicKey, pair) shouldBe Seq(counter.id())
    }

    withClue("orders list of submitted owner") {
      val expected = Seq(submitted.id())

      activeOrderIds(submitted.senderPublicKey) shouldBe expected
      allOrderIds(submitted.senderPublicKey) shouldBe expected

      activeOrderIdsByPair(submitted.senderPublicKey, pair) shouldBe expected
      allOrderIdsByPair(submitted.senderPublicKey, pair) shouldBe expected
    }
  }

  property("Buy WAVES order - filled with 2 steps, sell order - partial") {
    val pair       = AssetPair(None, mkAssetId("BTC"))
    val counter    = buy(pair, 0.0008, 100000000, matcherFee = Some(300001L))
    val submitted1 = sell(pair, 0.00075, 50000000, matcherFee = Some(300001L))
    val submitted2 = sell(pair, 0.0008, 80000000, matcherFee = Some(300001L))

    oh.process(OrderAdded(LimitOrder(counter)))
    val exec1 = OrderExecuted(LimitOrder(submitted1), LimitOrder(counter))
    oh.process(exec1)

    val counterInfo1    = oh.orderInfo(counter.id())
    val submitted1Info1 = oh.orderInfo(submitted1.id())

    exec1.counterRemainingAmount shouldBe counterInfo1.remaining
    exec1.counterRemainingFee shouldBe counterInfo1.remainingFee
    counterInfo1.status shouldBe LimitOrder.PartiallyFilled(50000000)

    exec1.submittedRemainingAmount shouldBe submitted1Info1.remaining
    exec1.submittedRemainingFee shouldBe submitted1Info1.remainingFee
    submitted1Info1.status shouldBe LimitOrder.Filled(50000000)

    oh.orderInfo(submitted2.id()).status shouldBe LimitOrder.NotFound

    val exec2 = OrderExecuted(LimitOrder(submitted2), exec1.counterRemaining)
    oh.processAll(exec2, OrderAdded(exec2.submittedRemaining))

    val counterInfo2 = oh.orderInfo(counter.id())
    withClue(s"counter: ${counter.idStr()}") {
      exec2.counterRemainingAmount shouldBe counterInfo2.remaining
      oh.orderInfo(counter.id()).status shouldBe LimitOrder.Filled(100000000)
    }

    oh.orderInfo(submitted1.id()).status shouldBe LimitOrder.Filled(50000000)

    val submitted2Info1 = oh.orderInfo(submitted2.id())
    exec2.submittedRemainingAmount shouldBe submitted2Info1.remaining
    exec2.submittedRemainingFee shouldBe submitted2Info1.remainingFee
    oh.orderInfo(submitted2.id()).status shouldBe LimitOrder.PartiallyFilled(50000000)

    oh.openVolume(counter.senderPublicKey, pair.priceAsset) shouldBe 0L
    oh.openVolume(counter.senderPublicKey, pair.amountAsset) shouldBe 0L
    activeOrderIds(counter.senderPublicKey) shouldBe empty

    oh.openVolume(submitted1.senderPublicKey, pair.priceAsset) shouldBe 0L
    oh.openVolume(submitted1.senderPublicKey, pair.amountAsset) shouldBe 0L
    activeOrderIds(submitted1.senderPublicKey) shouldBe empty

    withClue(s"account checks, ord3.senderPublicKey: ${submitted2.senderPublicKey}, ord3.order.id=${submitted2.idStr()}") {
      val lo             = LimitOrder(submitted2)
      val remainingSpend = lo.getSpendAmount - submitted2Info1.totalSpend(lo)
      oh.openVolume(submitted2.senderPublicKey, pair.amountAsset) shouldBe remainingSpend + submitted2Info1.remainingFee
      oh.openVolume(submitted2.senderPublicKey, pair.priceAsset) shouldBe 0L
      activeOrderIds(submitted2.senderPublicKey) shouldBe Seq(submitted2.id())
    }
  }

  property("WCT/BTC: sell - filled partially, buy - filled") {
    val pair      = AssetPair(mkAssetId("WCT"), mkAssetId("BTC"))
    val counter   = sell(pair, 0.12739213, 347, matcherFee = Some(300000L))
    val submitted = buy(pair, 0.12739213, 146, matcherFee = Some(300000L))

    oh.process(OrderAdded(LimitOrder(counter)))
    val exec = OrderExecuted(LimitOrder(submitted), LimitOrder(counter))
    oh.processAll(exec, OrderCanceled(exec.submittedRemaining, unmatchable = true))

    withClue(s"account checks, counter.senderPublicKey: ${counter.senderPublicKey}, counter.order.id=${counter.id()}") {
      oh.openVolume(counter.senderPublicKey, pair.amountAsset) shouldBe 205L
      oh.openVolume(counter.senderPublicKey, pair.priceAsset) shouldBe 0L
      oh.openVolume(counter.senderPublicKey, None) shouldBe counter.matcherFee - LimitOrder.getPartialFee(counter.matcherFee,
                                                                                                          counter.amount,
                                                                                                          exec.executedAmount)
    }

    withClue(s"account checks, submitted.senderPublicKey: ${submitted.senderPublicKey}, submitted.order.id=${submitted.id()}") {
      oh.openVolume(submitted.senderPublicKey, pair.amountAsset) shouldBe 0L
      oh.openVolume(submitted.senderPublicKey, pair.priceAsset) shouldBe 0L
      oh.openVolume(submitted.senderPublicKey, None) shouldBe 0L
    }
  }

  property("Buy USD order - filled, sell BTC order - filled") {
    val pair      = AssetPair(mkAssetId("USD"), mkAssetId("BTC"))
    val counter   = buy(pair, 0.001, 5000000, matcherFee = Some(1000L))
    val submitted = sell(pair, 0.00099908, 5000000, matcherFee = Some(1000L))

    oh.process(OrderAdded(LimitOrder(counter)))
    val exec = OrderExecuted(LimitOrder(submitted), LimitOrder(counter))
    oh.process(exec)

    withClue(s"account checks, submitted.senderPublicKey: ${submitted.senderPublicKey}, submitted.order.id=${submitted.id()}") {
      oh.openVolume(submitted.senderPublicKey, pair.amountAsset) shouldBe 0L
      oh.openVolume(submitted.senderPublicKey, pair.priceAsset) shouldBe 0L
      oh.openVolume(submitted.senderPublicKey, None) shouldBe 0L
    }
  }

  property("Sell ETH twice (filled, partial), buy WAVES order - filled") {
    val pair      = AssetPair(mkAssetId("ETH"), None)
    val counter1  = sell(pair, 0.003, 2864310, matcherFee = Some(300000L))
    val counter2  = sell(pair, 0.003, 7237977, matcherFee = Some(300000L))
    val submitted = buy(pair, 0.003, 4373667, matcherFee = Some(300000L))

    oh.processAll(OrderAdded(LimitOrder(counter1)), OrderAdded(LimitOrder(counter2)))
    val exec1 = OrderExecuted(LimitOrder(submitted), LimitOrder(counter1))
    oh.processAll(
      exec1,
      OrderCanceled(exec1.counterRemaining, unmatchable = true),
      OrderExecuted(exec1.submittedRemaining, LimitOrder(counter2))
    )

    withClue(s"account checks, submitted.senderPublicKey: ${submitted.senderPublicKey}, submitted.order.id=${submitted.id()}") {
      oh.openVolume(submitted.senderPublicKey, pair.amountAsset) shouldBe 0L
      oh.openVolume(submitted.senderPublicKey, pair.priceAsset) shouldBe 0L
    }
  }

  property("Sell EUR - partial, buy EUR order - filled") {
    val pair      = AssetPair(mkAssetId("EUR"), mkAssetId("USD"))
    val counter   = sell(pair, 0.001356, 57918, matcherFee = Some(300000L))
    val submitted = buy(pair, 0.003333, 46978, matcherFee = Some(300000L))

    oh.process(OrderAdded(LimitOrder(counter)))

    val exec = OrderExecuted(LimitOrder(submitted), LimitOrder(counter))
    oh.processAll(
      exec,
      OrderCanceled(exec.submittedRemaining, unmatchable = true),
      OrderCanceled(exec.counterRemaining, unmatchable = false) // Cancelled by user
    )

    withClue(s"account checks, submitted.senderPublicKey: ${submitted.senderPublicKey}, submitted.order.id=${submitted.id()}") {
      oh.openVolume(submitted.senderPublicKey, pair.amountAsset) shouldBe 0L
      oh.openVolume(submitted.senderPublicKey, pair.priceAsset) shouldBe 0L
      oh.openVolume(submitted.senderPublicKey, None) shouldBe 0L
    }
  }

  property("Total execution of two counter orders and the one submitted") {
    val pair = AssetPair(mkAssetId("Alice"), None)

    val counter1  = buy(pair, 190000000L, 150, matcherFee = Some(300000))
    val counter2  = buy(pair, 200000000L, 200, matcherFee = Some(300000))
    val submitted = sell(pair, 210000000L, 350, matcherFee = Some(300000))

    oh.processAll(OrderAdded(LimitOrder(counter1)), OrderAdded(LimitOrder(counter2)))
    val exec1 = OrderExecuted(LimitOrder(submitted), LimitOrder(counter1))
    oh.processAll(exec1, OrderAdded(exec1.submittedRemaining), OrderExecuted(exec1.submittedRemaining, LimitOrder(counter2)))

    oh.orderInfo(submitted.id()).status shouldBe LimitOrder.Filled(350)
  }

  property("Reserved balance should empty after full rounded execution") {
    val pair = AssetPair(mkAssetId("BTC"), mkAssetId("ETH"))

    val alicePk   = PrivateKeyAccount("alice".getBytes("utf-8"))
    val counter   = buy(pair, 0.00031887, 923431000L, matcherFee = Some(300000), sender = Some(alicePk))
    val bobPk     = PrivateKeyAccount("bob".getBytes("utf-8"))
    val submitted = sell(pair, 0.00031887, 223345000L, matcherFee = Some(300000), sender = Some(bobPk))

    oh.process(OrderAdded(LimitOrder(counter)))

    val exec = OrderExecuted(LimitOrder(submitted), LimitOrder(counter))
    exec.executedAmount shouldBe 223344937L
    oh.processAll(exec, OrderCanceled(exec.counterRemaining, unmatchable = false))

    withClue(s"Account of submitted order (id=${submitted.id()}) should have positive balances:") {
      DBUtils.reservedBalance(db, bobPk) shouldBe empty
      DBUtils.reservedBalance(db, alicePk) shouldBe empty
    }
  }

  property("Partially with own order") {
    val pk        = PrivateKeyAccount("private".getBytes("utf-8"))
    val pair      = AssetPair(None, mkAssetId("BTC"))
    val counter   = buy(pair, 0.0008, 100000000, Some(pk), Some(300000L))
    val submitted = sell(pair, 0.00079, 210000000, Some(pk), Some(300000L))

    oh.process(OrderAdded(LimitOrder(counter)))
    val exec = OrderExecuted(LimitOrder(submitted), LimitOrder(counter))
    oh.processAll(exec, OrderAdded(exec.submittedRemaining))

    val counterOrderInfo = oh.orderInfo(counter.id())
    withClue(s"counter: ${counter.idStr()}") {
      exec.counterRemainingAmount shouldBe 0L
      exec.counterRemainingAmount shouldBe counterOrderInfo.remaining

      exec.counterRemainingFee shouldBe 0L
      exec.counterRemainingFee shouldBe counterOrderInfo.remainingFee

      counterOrderInfo.status shouldBe LimitOrder.Filled(100000000)
    }

    val submittedOrderInfo = oh.orderInfo(submitted.id())
    withClue(s"submitted: ${submitted.idStr()}") {
      exec.submittedRemainingAmount shouldBe submitted.amount - exec.executedAmount
      exec.submittedRemainingAmount shouldBe submittedOrderInfo.remaining

      submittedOrderInfo.remainingFee shouldBe 157143L
      exec.submittedRemainingFee shouldBe submittedOrderInfo.remainingFee
    }

    val expectedAmountReserved = counterOrderInfo.remainingFee + submittedOrderInfo.remaining + submittedOrderInfo.remainingFee
    expectedAmountReserved shouldBe 110157143L

    oh.openVolume(pk, pair.amountAsset) shouldBe expectedAmountReserved
    oh.openVolume(pk, pair.priceAsset) shouldBe 0L

    withClue("orders list") {
      activeOrderIds(pk) shouldBe Seq(submitted.id())
      allOrderIds(pk) shouldBe Seq(submitted.id(), counter.id())

      activeOrderIdsByPair(pk, pair) shouldBe Seq(submitted.id())
      allOrderIdsByPair(pk, pair) shouldBe Seq(submitted.id(), counter.id())
    }
  }

  property("Submitted order Canceled during match") {
    val pk = PrivateKeyAccount("private".getBytes("utf-8"))

    val pair      = AssetPair(None, mkAssetId("USD"))
    val counter   = buy(pair, 0.1, 10000000L, matcherFee = Some(300000L))
    val submitted = sell(pair, 10, 100000L, Some(pk), Some(300000L))

    oh.processAll(OrderAdded(LimitOrder(counter)), OrderCanceled(LimitOrder(submitted), unmatchable = true))

    oh.openVolume(pk, pair.amountAsset) should be >= 0L
    oh.openVolume(pk, pair.priceAsset) should be >= 0L

    withClue("orders list of submitted owner") {
      activeOrderIds(pk) shouldBe empty
      allOrderIds(pk) shouldBe Seq(submitted.id())

      activeOrderIdsByPair(pk, pair) shouldBe empty
      allOrderIdsByPair(pk, pair) shouldBe Seq(submitted.id())
    }
  }

  property("Cancel buy order") {
    val ord1 = buy(pair, 0.0008, 100000000, matcherFee = Some(300000L))

    oh.processAll(OrderAdded(LimitOrder(ord1)), OrderCanceled(LimitOrder(ord1), unmatchable = false))

    oh.orderInfo(ord1.id()).status shouldBe LimitOrder.Cancelled(0)

    oh.openVolume(ord1.senderPublicKey, pair.amountAsset) shouldBe 0L
    oh.openVolume(ord1.senderPublicKey, pair.priceAsset) shouldBe 0L

    withClue("orders list") {
      val addr = ord1.senderPublicKey.toAddress

      activeOrderIds(addr) shouldBe empty
      allOrderIds(addr) shouldBe Seq(ord1.id())

      activeOrderIdsByPair(addr, pair) shouldBe empty
      allOrderIdsByPair(addr, pair) shouldBe Seq(ord1.id())
    }
  }

  property("Cancel sell order") {
    val ord1 = sell(pair, 0.0008, 100000000, matcherFee = Some(300000L))

    oh.process(OrderAdded(LimitOrder(ord1)))
    oh.process(OrderCanceled(LimitOrder(ord1), unmatchable = false))

    oh.orderInfo(ord1.id()).status shouldBe LimitOrder.Cancelled(0)

    oh.openVolume(ord1.senderPublicKey, pair.amountAsset) shouldBe 0L
    oh.openVolume(ord1.senderPublicKey, pair.priceAsset) shouldBe 0L
    activeOrderIds(ord1.senderPublicKey) shouldBe empty
  }

  property("Cancel partially executed order") {
    val pair      = AssetPair(None, mkAssetId("BTC"))
    val counter   = sell(pair, 0.0008, 2100000000, matcherFee = Some(300000L))
    val submitted = buy(pair, 0.00081, 1000000000, matcherFee = Some(300000L))

    oh.process(OrderAdded(LimitOrder(counter)))
    val exec1 = OrderExecuted(LimitOrder(submitted), LimitOrder(counter))
    oh.processAll(exec1, OrderCanceled(exec1.counter.partial(exec1.counterRemainingAmount, exec1.counterRemainingFee), unmatchable = false))

    oh.orderInfo(counter.id()).status shouldBe LimitOrder.Cancelled(1000000000)
    oh.orderInfo(submitted.id()).status shouldBe LimitOrder.Filled(1000000000)

    oh.openVolume(counter.senderPublicKey, pair.amountAsset) shouldBe 0L
    oh.openVolume(counter.senderPublicKey, pair.priceAsset) shouldBe 0L

    withClue("orders list of counter owner") {
      val addr = counter.senderPublicKey.toAddress

      activeOrderIds(addr) shouldBe empty
      allOrderIds(addr) shouldBe Seq(counter.id())

      activeOrderIdsByPair(addr, pair) shouldBe empty
      allOrderIdsByPair(addr, pair) shouldBe Seq(counter.id())
    }

    activeOrderIds(submitted.senderPublicKey) shouldBe empty
  }

  property("Delete order") {
    val pk        = PrivateKeyAccount("private".getBytes("utf-8"))
    val pair      = AssetPair(None, mkAssetId("BTC"))
    val counter   = buy(pair, 0.0008, 210000000, Some(pk), Some(300000L))
    val submitted = sell(pair, 0.00079, 100000000, Some(pk), Some(300000L))

    oh.process(OrderAdded(LimitOrder(counter)))
    val exec1 = OrderExecuted(LimitOrder(submitted), LimitOrder(counter))
    oh.process(exec1)

    val counterInfo = oh.orderInfo(counter.id())
    counterInfo.status shouldBe LimitOrder.PartiallyFilled(100000000)

    val submittedInfo = oh.orderInfo(submitted.id())
    submittedInfo.status shouldBe LimitOrder.Filled(100000000)

    oh.openVolume(pk, pair.amountAsset) shouldBe 0 // We receive 210000000 >> 300000 WAVES

    val counterLo             = LimitOrder(counter)
    val expectedPriceReserved = counterLo.getSpendAmount - counterInfo.totalSpend(counterLo)
    oh.openVolume(pk, pair.priceAsset) shouldBe expectedPriceReserved

    oh.deleteOrder(pk, counter.id()) shouldBe Left(LimitOrder.PartiallyFilled(100000000))
    oh.deleteOrder(pk, submitted.id()) shouldBe Right(())

    withClue("orders list") {
      val addr = pk.toAddress

      val expected = Seq(counter.id())

      activeOrderIds(addr) shouldBe expected
      allOrderIds(addr) shouldBe expected

      activeOrderIdsByPair(addr, pair) shouldBe expected
      allOrderIdsByPair(addr, pair) shouldBe expected
    }
  }

  property("Sorting by status then timestamp") {
    val pk   = PrivateKeyAccount("private".getBytes("utf-8"))
    val pair = AssetPair(None, mkAssetId("BTC"))
    val ord1 = buy(pair, 0.0008, 110000000, Some(pk), Some(300000L), Some(1L)) // Filled
    val ord2 = buy(pair, 0.0006, 120000000, Some(pk), Some(300000L), Some(2L)) // Accepted
    val ord3 = buy(pair, 0.0005, 130000000, Some(pk), Some(300000L), Some(3L)) // Canceled
    val ord4 = sell(pair, 0.00079, 2100000000, Some(pk), Some(300000L), Some(4L)) // Partial
    val ord5 = buy(pair, 0.0004, 130000000, Some(pk), Some(300000L), Some(45)) // Accepted

    oh.processAll(
      OrderAdded(LimitOrder(ord1)),
      OrderAdded(LimitOrder(ord2)),
      OrderAdded(LimitOrder(ord3))
    )
    val exec = OrderExecuted(LimitOrder(ord4), LimitOrder(ord1))
    oh.processAll(
      exec,
      OrderAdded(exec.submittedRemaining),
      OrderCanceled(LimitOrder(ord3), unmatchable = false),
      OrderAdded(LimitOrder(ord5))
    )

    allOrderIds(ord1.senderPublicKey) shouldBe
      Seq(ord5.id(), ord4.id(), ord2.id(), ord3.id(), ord1.id())

    activeOrderIds(ord1.senderPublicKey) shouldBe
      Seq(ord5.id(), ord4.id(), ord2.id())

    withClue("orders list") {
      val addr         = pk.toAddress
      val allOrders    = Seq(ord5, ord4, ord2, ord3, ord1).map(_.id())
      val activeOrders = Seq(ord5, ord4, ord2).map(_.id())

      activeOrderIds(addr) shouldBe activeOrders
      allOrderIds(addr) shouldBe allOrders

      activeOrderIdsByPair(addr, pair) shouldBe activeOrders
      allOrderIdsByPair(addr, pair) shouldBe allOrders
    }
  }

  property("History with more than max limit") {
    val pk   = PrivateKeyAccount("private".getBytes("utf-8"))
    val pair = AssetPair(None, mkAssetId("BTC"))
    val origOrders = (0 until matcherSettings.maxOrdersPerRequest).map { i =>
      val o = buy(pair, 0.0008 + 0.00001 * i, 100000000, Some(pk), Some(300000L), Some(100L + i))
      oh.process(OrderAdded(LimitOrder(o)))
      o
    }.toVector

    oh.process(OrderCanceled(LimitOrder(origOrders.last), unmatchable = false))

    val newOrder = buy(pair, 0.001, 100000000, Some(pk), Some(300000L), Some(1L))

    oh.process(OrderAdded(LimitOrder(newOrder)))

    withClue("orders list") {
      // 'last' is canceled, remove it
      val expectedActiveOrders = origOrders.init.reverse :+ newOrder
      activeOrderIds(pk) shouldBe expectedActiveOrders.map(_.id())

      // 'last' is canceled. It should be moved to the end of all orders' list, but it doesn't fit. So we remove it
      val expectedAllOrders = origOrders.init.reverse :+ newOrder
      val actualAllOrders   = allOrderIds(pk)
      actualAllOrders should have length matcherSettings.maxOrdersPerRequest
      actualAllOrders shouldBe expectedAllOrders.map(_.id())
    }
  }

  property("History with canceled order and more than max limit") {
    val pk   = PrivateKeyAccount("private".getBytes("utf-8"))
    val pair = AssetPair(None, mkAssetId("BTC"))
    val origOrders = (0 to matcherSettings.maxOrdersPerRequest).map { i =>
      val o = buy(pair, 0.0008 + 0.00001 * i, 100000000, Some(pk), Some(300000L), Some(100L + i))
      oh.process(OrderAdded(LimitOrder(o)))
      o
    }.toVector

    oh.process(OrderCanceled(LimitOrder(origOrders.last), unmatchable = false))

    withClue("orders list") {
      // 'last' is canceled, remove it
      activeOrderIds(pk) shouldBe origOrders.init.reverse.map(_.id())

      // 'last' is removed, because it doesn't fit in 'matcherSettings.maxOrdersPerRequest'
      val expectedAllOrders = origOrders.init.reverse
      val actualAllOrders   = allOrderIds(pk)
      actualAllOrders should have length matcherSettings.maxOrdersPerRequest
      allOrderIds(pk) shouldBe expectedAllOrders.map(_.id())
    }
  }

  property("History returns an expected number of orders") {
    val pk   = PrivateKeyAccount("private".getBytes("utf-8"))
    val pair = AssetPair(None, mkAssetId("BTC"))
    val orders = (0 to 3).map { i =>
      val o = buy(pair, 0.0008 + 0.00001 * i, 100000000, Some(pk), Some(300000L), Some(100 + i))
      oh.process(OrderAdded(LimitOrder(o)))
      o
    }

    orders.patch(2, Nil, 1).map(x => OrderCanceled(LimitOrder(x), unmatchable = false)).foreach(oh.process)
    oh.deleteOrder(pk, orders(1).id()) shouldBe 'right

    /*
     ----------------------------------------
    |   0      |    1    |    2   |     3    |
    +----------+---------+--------+----------+
    | Canceled | Deleted | Active | Canceled |
     ----------------------------------------
     */

    withClue("orders list") {
      val orders = DBUtils.ordersByAddress(db, pk.toAddress, activeOnly = false, 3)
      orders should have size 3
    }
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

    oh.processAll(OrderAdded(LimitOrder(ord1)), OrderAdded(LimitOrder(ord2)))

    DBUtils.reservedBalance(db, pk) shouldBe
      Map(
        None -> (2 * matcherFee - LimitOrder(ord1).getReceiveAmount - LimitOrder(ord2).getReceiveAmount),
        ass1 -> ord1.amount,
        ass2 -> ord2.amount
      )
  }
}

private object OrderHistorySpecification {
  final implicit class OrderHistoryOps(val self: OrderHistory) extends AnyVal {
    def processAll(events: Events.Event*): Unit = events.foreach {
      case e: OrderAdded    => self.process(e)
      case e: OrderExecuted => self.process(e)
      case e: OrderCanceled => self.process(e)
    }
  }
}

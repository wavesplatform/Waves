package com.wavesplatform.matcher.model

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.testkit.TestKit
import akka.util.Timeout
import com.wavesplatform.NTPTime
import com.wavesplatform.account.{Address, PrivateKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.matcher.model.Events.{OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.matcher.{AddressActor, MatcherTestData}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import org.scalatest._

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.ClassTag

class OrderHistoryBalanceSpecification
    extends TestKit(ActorSystem())
    with PropSpecLike
    with Matchers
    with MatcherTestData
    with BeforeAndAfterEach
    with NTPTime {

  import OrderHistoryBalanceSpecification._

  private val WctBtc   = AssetPair(mkAssetId("WCT"), mkAssetId("BTC"))
  private val WavesBtc = AssetPair(Waves, mkAssetId("BTC"))

  private var oh = new OrderHistoryStub(system, ntpTime)
  override def beforeEach(): Unit = {
    super.beforeEach()
    oh = new OrderHistoryStub(system, ntpTime)
  }

  def openVolume(address: Address, asset: Asset): Long = oh.ref(address).openVolume(asset)

  def activeOrderIds(sender: Address): Seq[ByteStr]                        = oh.ref(sender).activeOrderIds
  def allOrderIds(sender: Address): Seq[ByteStr]                           = oh.ref(sender).allOrderIds
  def activeOrderIdsByPair(sender: Address, pair: AssetPair): Seq[ByteStr] = oh.ref(sender).activeOrderIdsByPair(pair)
  def allOrderIdsByPair(sender: Address, pair: AssetPair): Seq[ByteStr]    = oh.ref(sender).allOrderIdsByPair(pair)
  def orderStatus(orderId: ByteStr)                                        = oh.ref(orderId).orderStatus(orderId)

  property("New buy order added") {
    val ord = buy(WctBtc, 10000, 0.0007)

    val lo = LimitOrder(ord)
    oh.process(OrderAdded(lo))

    withClue("info") {
      orderStatus(ord.id()) shouldBe OrderStatus.Accepted
    }

    withClue("reserved assets") {
      openVolume(ord.senderPublicKey, WctBtc.amountAsset) shouldBe 0L
      openVolume(ord.senderPublicKey, WctBtc.priceAsset) shouldBe 7L
      openVolume(ord.senderPublicKey, Waves) shouldBe ord.matcherFee
    }

    withClue("orders list") {
      val expected = Seq(ord.id())

      activeOrderIds(ord.senderPublicKey) shouldBe expected
      allOrderIds(ord.senderPublicKey) shouldBe expected

      activeOrderIdsByPair(ord.senderPublicKey, WctBtc) shouldBe expected
      allOrderIdsByPair(ord.senderPublicKey, WctBtc) shouldBe expected
    }
  }

  property("New sell order added") {
    val ord = sell(WctBtc, 10000, 0.0007)

    oh.process(OrderAdded(LimitOrder(ord)))

    withClue("info") {
      orderStatus(ord.id()) shouldBe OrderStatus.Accepted
    }

    withClue("reserved assets") {
      openVolume(ord.senderPublicKey, WctBtc.amountAsset) shouldBe 10000L
      openVolume(ord.senderPublicKey, WctBtc.priceAsset) shouldBe 0L
      openVolume(ord.senderPublicKey, Waves) shouldBe ord.matcherFee
    }

    withClue("orders list") {
      val expected = Seq(ord.id())

      activeOrderIds(ord.senderPublicKey) shouldBe expected
      allOrderIds(ord.senderPublicKey) shouldBe expected

      activeOrderIdsByPair(ord.senderPublicKey, WctBtc) shouldBe expected
      allOrderIdsByPair(ord.senderPublicKey, WctBtc) shouldBe expected
    }
  }

  property("New buy WAVES order added") {
    val ord = buy(WavesBtc, 1000, 0.008, matcherFee = Some(3000))
    val lo  = LimitOrder(ord)

    oh.process(OrderAdded(lo))

    withClue("info") {
      orderStatus(ord.id()) shouldBe OrderStatus.Accepted
    }

    withClue("reserved assets considering amount of received WAVES") {
      openVolume(ord.senderPublicKey, WavesBtc.amountAsset) shouldBe 2000L
      openVolume(ord.senderPublicKey, WavesBtc.priceAsset) shouldBe 8L
    }

    activeOrderIds(ord.senderPublicKey) shouldBe Seq(ord.id())
  }

  property("New sell WAVES order added") {
    val ord = sell(WavesBtc, 10000, 0.0008)
    val lo  = LimitOrder(ord)

    oh.process(OrderAdded(lo))
    orderStatus(ord.id()) shouldBe OrderStatus.Accepted
    openVolume(ord.senderPublicKey, WavesBtc.amountAsset) shouldBe 10000L + ord.matcherFee
    openVolume(ord.senderPublicKey, WavesBtc.priceAsset) shouldBe 0L

    activeOrderIds(ord.senderPublicKey) shouldBe Seq(ord.id())
  }

  property("Should not reserve fee, if seller receives more WAVES than total fee in sell order") {
    val pair = AssetPair(mkAssetId("BTC"), Waves)
    val ord  = sell(pair, 100000, 0.01, matcherFee = Some(1000L))

    oh.process(OrderAdded(LimitOrder(ord)))
    orderStatus(ord.id()) shouldBe OrderStatus.Accepted

    openVolume(ord.senderPublicKey, pair.priceAsset) shouldBe 0L
  }

  property("Should not reserve fee, if buyer receives more WAVES than total fee in buy order") {
    val ord = buy(WavesBtc, 100000, 0.0007, matcherFee = Some(1000L))

    oh.process(OrderAdded(LimitOrder(ord)))
    orderStatus(ord.id()) shouldBe OrderStatus.Accepted

    openVolume(ord.senderPublicKey, WavesBtc.amountAsset) shouldBe 0L
  }

  property("Two sell orders added") {
    val pk   = PrivateKeyAccount("private".getBytes("utf-8"))
    val ord1 = sell(WavesBtc, 10000, 0.0005, Some(pk), matcherFee = Some(30000L), ts = Some(System.currentTimeMillis()))
    val ord2 = sell(WavesBtc, 16000, 0.0008, Some(pk), matcherFee = Some(30000L), ts = Some(System.currentTimeMillis() + 1))

    oh.processAll(OrderAdded(LimitOrder(ord1)), OrderAdded(LimitOrder(ord2)))

    withClue("all orders accepted") {
      orderStatus(ord1.id()) shouldBe OrderStatus.Accepted
      orderStatus(ord2.id()) shouldBe OrderStatus.Accepted
    }

    withClue("correction was used to reserve assets") {
      openVolume(ord1.senderPublicKey, WavesBtc.amountAsset) shouldBe ord1.amount + ord1.matcherFee + ord2.amount + ord2.matcherFee
      openVolume(ord1.senderPublicKey, WavesBtc.priceAsset) shouldBe 0L
    }

    withClue("orders list") {
      val expected = Seq(ord2.id(), ord1.id())

      activeOrderIds(ord1.senderPublicKey) shouldBe expected
      allOrderIds(ord1.senderPublicKey) shouldBe expected

      activeOrderIdsByPair(ord1.senderPublicKey, WavesBtc) shouldBe expected
      allOrderIdsByPair(ord1.senderPublicKey, WavesBtc) shouldBe expected
    }
  }

  property("Buy WAVES order filled exactly") {
    val counter   = buy(WavesBtc, 100000, 0.0008, matcherFee = Some(2000L))
    val submitted = sell(WavesBtc, 100000, 0.0007, matcherFee = Some(1000L))

    oh.process(OrderAdded(LimitOrder(counter)))

    val exec = OrderExecuted(LimitOrder(submitted), LimitOrder(counter))
    oh.process(exec)

    withClue("executed exactly") {
      exec.executedAmount shouldBe counter.amount
      orderStatus(counter.id()) shouldBe OrderStatus.Filled(exec.executedAmount)
      orderStatus(submitted.id()) shouldBe OrderStatus.Filled(exec.executedAmount)
    }

    withClue(s"has no reserved assets, counter.senderPublicKey: ${counter.senderPublicKey}, counter.order.id=${counter.idStr()}") {
      openVolume(counter.senderPublicKey, WavesBtc.amountAsset) shouldBe 0L
      openVolume(counter.senderPublicKey, WavesBtc.priceAsset) shouldBe 0L
    }

    withClue(s"has no reserved assets, submitted.senderPublicKey: ${submitted.senderPublicKey}, submitted.order.id=${submitted.idStr()}") {
      openVolume(submitted.senderPublicKey, WavesBtc.amountAsset) shouldBe 0L
      openVolume(submitted.senderPublicKey, WavesBtc.priceAsset) shouldBe 0L
    }

    withClue(s"orders list of counter owner ${counter.sender}") {
      activeOrderIds(counter.senderPublicKey) shouldBe empty
      allOrderIds(counter.senderPublicKey) shouldBe Seq(counter.id())

      activeOrderIdsByPair(counter.senderPublicKey, WavesBtc) shouldBe empty
      allOrderIdsByPair(counter.senderPublicKey, WavesBtc) shouldBe Seq(counter.id())
    }

    withClue(s"orders list of submitted owner ${submitted.sender}") {
      activeOrderIds(submitted.senderPublicKey) shouldBe empty
      allOrderIds(submitted.senderPublicKey) shouldBe Seq(submitted.id())

      activeOrderIdsByPair(submitted.senderPublicKey, WavesBtc) shouldBe empty
      allOrderIdsByPair(submitted.senderPublicKey, WavesBtc) shouldBe Seq(submitted.id())
    }
  }

  property("Buy WAVES order filled with remainder") {
    val counter   = sell(WavesBtc, 840340L, 0.00000238, matcherFee = Some(300000L))
    val submitted = buy(WavesBtc, 425532L, 0.00000238, matcherFee = Some(300000L))

    val counterLo = LimitOrder(counter)
    oh.process(OrderAdded(counterLo))
    withClue(s"account checks, counter.senderPublicKey: ${counter.senderPublicKey}, counter.order.id=${counter.idStr()}") {
      openVolume(counter.senderPublicKey, WavesBtc.priceAsset) shouldBe 0L
      activeOrderIds(counter.senderPublicKey) shouldBe Seq(counter.id())
    }

    val exec = OrderExecuted(LimitOrder(submitted), LimitOrder(counter))
    exec.executedAmount shouldBe 420169L

    oh.process(exec)
    withClue(s"counter.order.id=${submitted.idStr()}") {

      exec.counterRemainingAmount shouldBe 420171L
      exec.counterRemainingAmount shouldBe counter.amount - exec.executedAmount

      exec.counterRemainingFee shouldBe 150001L

      orderStatus(counter.id()) shouldBe OrderStatus.PartiallyFilled(exec.executedAmount)
    }

    withClue(s"submitted.order.id=${counter.idStr()}") {
      exec.submittedRemainingAmount shouldBe 5363L
      exec.submittedRemainingAmount shouldBe submitted.amount - exec.executedAmount

      exec.submittedRemainingFee shouldBe 3781L
      orderStatus(submitted.id()) shouldBe OrderStatus.Filled(exec.executedAmount)
    }

    withClue(s"account checks, counter.senderPublicKey: ${counter.senderPublicKey}, counter.order.id=${counter.idStr()}") {
      openVolume(counter.senderPublicKey, WavesBtc.priceAsset) shouldBe 0L
    }

    withClue(s"account checks, submitted.senderPublicKey: ${submitted.senderPublicKey}, submitted.order.id=${submitted.idStr()}") {
      openVolume(submitted.senderPublicKey, WavesBtc.amountAsset) shouldBe 0L
      openVolume(submitted.senderPublicKey, WavesBtc.priceAsset) shouldBe 0L
    }

    withClue("orders list of counter owner") {
      val expected = Seq(counter.id())

      activeOrderIds(counter.senderPublicKey) shouldBe expected
      allOrderIds(counter.senderPublicKey) shouldBe expected

      activeOrderIdsByPair(counter.senderPublicKey, WavesBtc) shouldBe expected
      allOrderIdsByPair(counter.senderPublicKey, WavesBtc) shouldBe expected
    }

    withClue("orders list of submitted owner") {
      activeOrderIds(submitted.senderPublicKey) shouldBe empty
      allOrderIds(submitted.senderPublicKey) shouldBe Seq(submitted.id())

      activeOrderIdsByPair(submitted.senderPublicKey, WavesBtc) shouldBe empty
      allOrderIdsByPair(submitted.senderPublicKey, WavesBtc) shouldBe Seq(submitted.id())
    }
  }

  property("Sell WAVES order - filled, buy order - partial") {
    val counter   = sell(WavesBtc, 100000000, 0.0008, matcherFee = Some(2000L))
    val submitted = buy(WavesBtc, 120000000, 0.00085, matcherFee = Some(1000L))

    oh.process(OrderAdded(LimitOrder(counter)))
    val exec = OrderExecuted(LimitOrder(submitted), LimitOrder(counter))
    oh.processAll(exec, OrderAdded(exec.submittedRemaining))

    withClue(s"counter: ${counter.idStr()}") {
      exec.counterRemainingAmount shouldBe 0L
      exec.counterRemainingFee shouldBe 0L

      orderStatus(counter.id()) shouldBe OrderStatus.Filled(100000000)
    }

    withClue(s"submitted: ${submitted.idStr()}") {
      exec.submittedRemainingAmount shouldBe 20000000L
      exec.submittedRemainingFee shouldBe 167L

      orderStatus(submitted.id()) shouldBe OrderStatus.PartiallyFilled(100000000)
    }

    withClue(s"account checks, submitted.senderPublicKey: ${submitted.senderPublicKey}, submitted.order.id=${submitted.idStr()}") {
//      openVolume(submitted.senderPublicKey, pair.amountAsset) shouldBe
//        math.max(0L,
//                 OrderInfo.safeSum(LimitOrder.getPartialFee(submitted.matcherFee, submitted.amount, submitted.amount - counter.amount), -20000000L))
      openVolume(submitted.senderPublicKey, WavesBtc.priceAsset) shouldBe (BigDecimal(0.00085) * 20000000L).toLong
    }

    withClue("orders list of counter owner") {
      activeOrderIds(counter.senderPublicKey) shouldBe empty
      allOrderIds(counter.senderPublicKey) shouldBe Seq(counter.id())

      activeOrderIdsByPair(counter.senderPublicKey, WavesBtc) shouldBe empty
      allOrderIdsByPair(counter.senderPublicKey, WavesBtc) shouldBe Seq(counter.id())
    }

    withClue("orders list of submitted owner") {
      val expected = Seq(submitted.id())

      activeOrderIds(submitted.senderPublicKey) shouldBe expected
      allOrderIds(submitted.senderPublicKey) shouldBe expected

      activeOrderIdsByPair(submitted.senderPublicKey, WavesBtc) shouldBe expected
      allOrderIdsByPair(submitted.senderPublicKey, WavesBtc) shouldBe expected
    }
  }

  property("Buy WAVES order - filled with 2 steps, sell order - partial") {
    val counter    = buy(WavesBtc, 100000000, 0.0008, matcherFee = Some(300001L))
    val submitted1 = sell(WavesBtc, 50000000, 0.00075, matcherFee = Some(300001L))
    val submitted2 = sell(WavesBtc, 80000000, 0.0008, matcherFee = Some(300001L))

    oh.process(OrderAdded(LimitOrder(counter)))
    val exec1 = OrderExecuted(LimitOrder(submitted1), LimitOrder(counter))
    oh.process(exec1)

    orderStatus(counter.id()) shouldBe OrderStatus.PartiallyFilled(50000000)
    orderStatus(submitted1.id()) shouldBe OrderStatus.Filled(50000000)

    val exec2 = OrderExecuted(LimitOrder(submitted2), exec1.counterRemaining)
    oh.processAll(exec2, OrderAdded(exec2.submittedRemaining))

    withClue(s"counter: ${counter.idStr()}") {
      orderStatus(counter.id()) shouldBe OrderStatus.Filled(100000000)
    }

    orderStatus(submitted1.id()) shouldBe OrderStatus.Filled(50000000)
    orderStatus(submitted2.id()) shouldBe OrderStatus.PartiallyFilled(50000000)

    openVolume(counter.senderPublicKey, WavesBtc.priceAsset) shouldBe 0L
    openVolume(counter.senderPublicKey, WavesBtc.amountAsset) shouldBe 0L
    activeOrderIds(counter.senderPublicKey) shouldBe empty

    openVolume(submitted1.senderPublicKey, WavesBtc.priceAsset) shouldBe 0L
    openVolume(submitted1.senderPublicKey, WavesBtc.amountAsset) shouldBe 0L
    activeOrderIds(submitted1.senderPublicKey) shouldBe empty

    withClue(s"account checks, ord3.senderPublicKey: ${submitted2.senderPublicKey}, ord3.order.id=${submitted2.idStr()}") {
      openVolume(submitted2.senderPublicKey, WavesBtc.priceAsset) shouldBe 0L
      activeOrderIds(submitted2.senderPublicKey) shouldBe Seq(submitted2.id())
    }
  }

  property("WCT/BTC: sell - filled partially, buy - filled") {
    val pair      = AssetPair(mkAssetId("WCT"), mkAssetId("BTC"))
    val counter   = sell(pair, 347, 0.12739213, matcherFee = Some(300000L))
    val submitted = buy(pair, 146, 0.12739213, matcherFee = Some(300000L))

    oh.process(OrderAdded(LimitOrder(counter)))
    val exec = OrderExecuted(LimitOrder(submitted), LimitOrder(counter))
    oh.processAll(exec, OrderCanceled(exec.submittedRemaining, unmatchable = true))

    withClue(s"account checks, counter.senderPublicKey: ${counter.senderPublicKey}, counter.order.id=${counter.id()}") {
      openVolume(counter.senderPublicKey, pair.amountAsset) shouldBe 205L
      openVolume(counter.senderPublicKey, pair.priceAsset) shouldBe 0L
      openVolume(counter.senderPublicKey, Waves) shouldBe counter.matcherFee - LimitOrder.partialFee(counter.matcherFee,
                                                                                                     counter.amount,
                                                                                                     exec.executedAmount)
    }

    withClue(s"account checks, submitted.senderPublicKey: ${submitted.senderPublicKey}, submitted.order.id=${submitted.id()}") {
      openVolume(submitted.senderPublicKey, pair.amountAsset) shouldBe 0L
      openVolume(submitted.senderPublicKey, pair.priceAsset) shouldBe 0L
      openVolume(submitted.senderPublicKey, Waves) shouldBe 0L
    }
  }

  property("Buy USD order - filled, sell BTC order - filled") {
    val pair      = AssetPair(mkAssetId("USD"), mkAssetId("BTC"))
    val counter   = buy(pair, 5000000, 0.001, matcherFee = Some(1000L))
    val submitted = sell(pair, 5000000, 0.00099908, matcherFee = Some(1000L))

    oh.process(OrderAdded(LimitOrder(counter)))
    val exec = OrderExecuted(LimitOrder(submitted), LimitOrder(counter))
    oh.process(exec)

    withClue(s"account checks, submitted.senderPublicKey: ${submitted.senderPublicKey}, submitted.order.id=${submitted.id()}") {
      openVolume(submitted.senderPublicKey, pair.amountAsset) shouldBe 0L
      openVolume(submitted.senderPublicKey, pair.priceAsset) shouldBe 0L
      openVolume(submitted.senderPublicKey, Waves) shouldBe 0L
    }
  }

  property("Sell ETH twice (filled, partial), buy WAVES order - filled") {
    val pair      = AssetPair(mkAssetId("ETH"), Waves)
    val counter1  = sell(pair, 2864310, 0.003, matcherFee = Some(300000L))
    val counter2  = sell(pair, 7237977, 0.003, matcherFee = Some(300000L))
    val submitted = buy(pair, 4373667, 0.003, matcherFee = Some(300000L))

    oh.processAll(OrderAdded(LimitOrder(counter1)), OrderAdded(LimitOrder(counter2)))
    val exec1 = OrderExecuted(LimitOrder(submitted), LimitOrder(counter1))
    oh.processAll(
      exec1,
      OrderCanceled(exec1.counterRemaining, unmatchable = true),
      OrderExecuted(exec1.submittedRemaining, LimitOrder(counter2))
    )

    withClue(s"account checks, submitted.senderPublicKey: ${submitted.senderPublicKey}, submitted.order.id=${submitted.id()}") {
      openVolume(submitted.senderPublicKey, pair.amountAsset) shouldBe 0L
      openVolume(submitted.senderPublicKey, pair.priceAsset) shouldBe 0L
    }
  }

  property("Sell EUR - partial, buy EUR order - filled") {
    val pair      = AssetPair(mkAssetId("EUR"), mkAssetId("USD"))
    val counter   = sell(pair, 57918, 0.001356, matcherFee = Some(300000L))
    val submitted = buy(pair, 46978, 0.003333, matcherFee = Some(300000L))

    oh.process(OrderAdded(LimitOrder(counter)))

    val exec = OrderExecuted(LimitOrder(submitted), LimitOrder(counter))
    oh.processAll(
      exec,
      OrderCanceled(exec.submittedRemaining, unmatchable = true),
      OrderCanceled(exec.counterRemaining, unmatchable = false) // Cancelled by user
    )

    withClue(s"account checks, submitted.senderPublicKey: ${submitted.senderPublicKey}, submitted.order.id=${submitted.id()}") {
      openVolume(submitted.senderPublicKey, pair.amountAsset) shouldBe 0L
      openVolume(submitted.senderPublicKey, pair.priceAsset) shouldBe 0L
      openVolume(submitted.senderPublicKey, Waves) shouldBe 0L
    }
  }

  property("Total execution of two counter orders and the one submitted") {
    val pair = AssetPair(mkAssetId("Alice"), Waves)

    val counter1  = buy(pair, 150, 190000000L, matcherFee = Some(300000))
    val counter2  = buy(pair, 200, 200000000L, matcherFee = Some(300000))
    val submitted = sell(pair, 350, 210000000L, matcherFee = Some(300000))

    oh.processAll(OrderAdded(LimitOrder(counter1)), OrderAdded(LimitOrder(counter2)))
    val exec1 = OrderExecuted(LimitOrder(submitted), LimitOrder(counter1))
    oh.processAll(exec1, OrderAdded(exec1.submittedRemaining), OrderExecuted(exec1.submittedRemaining, LimitOrder(counter2)))

    orderStatus(submitted.id()) shouldBe OrderStatus.Filled(350)
  }

  property("Partially with own order") {
    val pk        = PrivateKeyAccount("private".getBytes("utf-8"))
    val counter   = buy(WavesBtc, 100000000, 0.0008, Some(pk), Some(300000L))
    val submitted = sell(WavesBtc, 210000000, 0.00079, Some(pk), Some(300000L))

    oh.process(OrderAdded(LimitOrder(counter)))
    val exec = OrderExecuted(LimitOrder(submitted), LimitOrder(counter))
    oh.processAll(exec, OrderAdded(exec.submittedRemaining))

    withClue(s"counter: ${counter.idStr()}") {
      exec.counterRemainingAmount shouldBe 0L
      exec.counterRemainingFee shouldBe 0L
      orderStatus(counter.id()) shouldBe OrderStatus.Filled(100000000)
    }

    withClue(s"submitted: ${submitted.idStr()}") {
      exec.submittedRemainingAmount shouldBe submitted.amount - exec.executedAmount
    }

    openVolume(pk, WavesBtc.amountAsset) shouldBe 110157143L
    openVolume(pk, WavesBtc.priceAsset) shouldBe 0L

    withClue("orders list") {
      activeOrderIds(pk) shouldBe Seq(submitted.id())
      allOrderIds(pk) shouldBe Seq(submitted.id(), counter.id())

      activeOrderIdsByPair(pk, WavesBtc) shouldBe Seq(submitted.id())
      allOrderIdsByPair(pk, WavesBtc) shouldBe Seq(submitted.id(), counter.id())
    }
  }

  property("Cancel buy order") {
    val ord1 = buy(WctBtc, 100000000, 0.0008, matcherFee = Some(300000L))

    oh.processAll(OrderAdded(LimitOrder(ord1)), OrderCanceled(LimitOrder(ord1), unmatchable = false))

    orderStatus(ord1.id()) shouldBe OrderStatus.Cancelled(0)

    openVolume(ord1.senderPublicKey, WctBtc.amountAsset) shouldBe 0L
    openVolume(ord1.senderPublicKey, WctBtc.priceAsset) shouldBe 0L

    withClue("orders list") {
      val addr = ord1.senderPublicKey.toAddress

      activeOrderIds(addr) shouldBe empty
      allOrderIds(addr) shouldBe Seq(ord1.id())

      activeOrderIdsByPair(addr, WctBtc) shouldBe empty
      allOrderIdsByPair(addr, WctBtc) shouldBe Seq(ord1.id())
    }
  }

  property("Cancel sell order") {
    val ord1 = sell(WctBtc, 100000000, 0.0008, matcherFee = Some(300000L))

    oh.process(OrderAdded(LimitOrder(ord1)))
    oh.process(OrderCanceled(LimitOrder(ord1), unmatchable = false))

    orderStatus(ord1.id()) shouldBe OrderStatus.Cancelled(0)

    openVolume(ord1.senderPublicKey, WctBtc.amountAsset) shouldBe 0L
    openVolume(ord1.senderPublicKey, WctBtc.priceAsset) shouldBe 0L
    activeOrderIds(ord1.senderPublicKey) shouldBe empty
  }

  property("Cancel partially executed order") {
    val counter   = sell(WavesBtc, 2100000000, 0.0008, matcherFee = Some(300000L))
    val submitted = buy(WavesBtc, 1000000000, 0.00081, matcherFee = Some(300000L))

    oh.process(OrderAdded(LimitOrder(counter)))
    val exec1 = OrderExecuted(LimitOrder(submitted), LimitOrder(counter))
    oh.processAll(exec1, OrderCanceled(exec1.counter.partial(exec1.counterRemainingAmount, exec1.counterRemainingFee), unmatchable = false))

    orderStatus(counter.id()) shouldBe OrderStatus.Cancelled(1000000000)
    orderStatus(submitted.id()) shouldBe OrderStatus.Filled(1000000000)

    openVolume(counter.senderPublicKey, WavesBtc.amountAsset) shouldBe 0L
    openVolume(counter.senderPublicKey, WavesBtc.priceAsset) shouldBe 0L

    withClue("orders list of counter owner") {
      val addr = counter.senderPublicKey.toAddress

      activeOrderIds(addr) shouldBe empty
      allOrderIds(addr) shouldBe Seq(counter.id())

      activeOrderIdsByPair(addr, WavesBtc) shouldBe empty
      allOrderIdsByPair(addr, WavesBtc) shouldBe Seq(counter.id())
    }

    activeOrderIds(submitted.senderPublicKey) shouldBe empty
  }

  property("Sorting by status then timestamp") {
    val pk   = PrivateKeyAccount("private".getBytes("utf-8"))
    val ord1 = buy(WavesBtc, 110000000, 0.0008, Some(pk), Some(300000L), Some(1L)) // Filled
    val ord2 = buy(WavesBtc, 120000000, 0.0006, Some(pk), Some(300000L), Some(2L)) // Accepted
    val ord3 = buy(WavesBtc, 130000000, 0.0005, Some(pk), Some(300000L), Some(3L)) // Canceled
    val ord4 = sell(WavesBtc, 2100000000, 0.00079, Some(pk), Some(300000L), Some(4L)) // Partial
    val ord5 = buy(WavesBtc, 130000000, 0.0004, Some(pk), Some(300000L), Some(45)) // Accepted

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

      activeOrderIdsByPair(addr, WavesBtc) shouldBe activeOrders
      allOrderIdsByPair(addr, WavesBtc) shouldBe allOrders
    }
  }

  property("History with more than max limit") {
    val pk = PrivateKeyAccount("private".getBytes("utf-8"))
    val origOrders = (0 until matcherSettings.maxOrdersPerRequest).map { i =>
      val o = buy(WavesBtc, 100000000, 0.0008 + 0.00001 * i, Some(pk), Some(300000L), Some(100L + i))
      oh.process(OrderAdded(LimitOrder(o)))
      o
    }.toVector

    oh.process(OrderCanceled(LimitOrder(origOrders.last), unmatchable = false))

    val newOrder = buy(WavesBtc, 100000000, 0.001, Some(pk), Some(300000L), Some(1L))

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
    val pk = PrivateKeyAccount("private".getBytes("utf-8"))
    val origOrders = (0 to matcherSettings.maxOrdersPerRequest).map { i =>
      val o = buy(WavesBtc, 100000000, 0.0008 + 0.00001 * i, Some(pk), Some(300000L), Some(100L + i))
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

  property("History by pair - added orders more than history by pair limit (200 active)") {
    val pk = PrivateKeyAccount("private".getBytes("utf-8"))

    val orders = (1 to MaxElements).map { i =>
      val o = buy(WavesBtc, 100000000, 0.0008 + 0.00001 * i, Some(pk), Some(300000L), Some(100L + i))
      oh.process(OrderAdded(LimitOrder(o)))
      o
    }.toVector

    val expectedIds = orders.map(_.id()).reverse

    withClue("common") {
      val allIds = allOrderIds(pk)
      allIds shouldBe expectedIds
      activeOrderIds(pk) shouldBe expectedIds
    }

    withClue("pair") {
      // Even expectedIds.size < pair.MaxElements!
      allOrderIdsByPair(pk, WavesBtc) shouldBe expectedIds
      activeOrderIdsByPair(pk, WavesBtc) shouldBe expectedIds
    }
  }

  property("History by pair - added and canceled orders both more than history by pair limit (200 active, 10 canceled)") {
    val pk = PrivateKeyAccount("private".getBytes("utf-8"))

    val allOrders = (1 to MaxElements + 10).map { i =>
      val o = buy(WavesBtc, 100000000, 0.0008 + 0.00001 * i, Some(pk), Some(300000L), Some(100L + i))
      oh.processAll(OrderAdded(LimitOrder(o)))
      o
    }.toVector

    val (ordersToCancel, activeOrders) = allOrders.splitAt(MaxElements)
    ordersToCancel.foreach(o => oh.process(OrderCanceled(LimitOrder(o), unmatchable = false)))
    val expectedActiveOrderIds = activeOrders.map(_.id()).reverse

    withClue("common") {
      val expectedIds = allOrders.takeRight(MaxElements).map(_.id()).reverse
      val allIds      = allOrderIds(pk)
      allIds shouldBe expectedIds
      activeOrderIds(pk) shouldBe expectedActiveOrderIds
    }

    withClue("pair") {
      val expectedIds = allOrders.takeRight(MaxElements).map(_.id()).reverse
      val pair1Ids    = allOrderIdsByPair(pk, WavesBtc)
      pair1Ids shouldBe expectedIds
      activeOrderIdsByPair(pk, WavesBtc) shouldBe expectedActiveOrderIds
    }
  }

  property("History by pair contains more elements than in common") {
    val pk    = PrivateKeyAccount("private".getBytes("utf-8"))
    val pair1 = WavesBtc
    val pair2 = AssetPair(Waves, mkAssetId("ETH"))

    // 1. Place and cancel active.MaxElements orders

    val pair1Orders = (1 to MaxElements).map { i =>
      val o = buy(pair1, 100000000, 0.0008 + 0.00001 * i, Some(pk), Some(300000L), Some(100L + i))
      oh.process(OrderAdded(LimitOrder(o)))
      o
    }.toVector

    pair1Orders.foreach(o => oh.process(OrderCanceled(LimitOrder(o), unmatchable = false)))

    withClue("after 1 step") {
      activeOrderIds(pk) shouldBe empty
      activeOrderIdsByPair(pk, pair1) shouldBe empty
      activeOrderIdsByPair(pk, pair2) shouldBe empty

      val expectedIds = pair1Orders.map(_.id()).reverse

      withClue("common") {
        allOrderIds(pk) shouldBe expectedIds.take(MaxElements)
      }

      withClue("pair1") {
        allOrderIdsByPair(pk, pair1) shouldBe expectedIds.take(MaxElements)
      }

      withClue("pair2") {
        val pair2Ids = allOrderIdsByPair(pk, pair2)
        pair2Ids shouldBe empty
      }
    }

    // 2. Place and cancel 10 orders in pair2

    val pair2Orders = (1 to 10).map { i =>
      val o = buy(pair2, 100000000, 0.0008 + 0.00001 * i, Some(pk), Some(300000L), Some(1000L + i))
      oh.process(OrderAdded(LimitOrder(o)))
      o
    }.toVector

    pair2Orders.foreach(o => oh.process(OrderCanceled(LimitOrder(o), unmatchable = false)))

    withClue("after 2 step") {
      activeOrderIds(pk) shouldBe empty
      activeOrderIdsByPair(pk, pair1) shouldBe empty
      activeOrderIdsByPair(pk, pair2) shouldBe empty

      withClue("common") {
        val allIds      = allOrderIds(pk)
        val expectedIds = pair2Orders.map(_.id()).reverse ++ pair1Orders.map(_.id()).reverse.take(MaxElements - pair2Orders.size)
        allIds shouldBe expectedIds
      }

      withClue("pair1") {
        val pair1Ids = allOrderIdsByPair(pk, pair1)
        pair1Ids shouldBe pair1Orders.map(_.id()).reverse.take(MaxElements)
      }

      withClue("pair2") {
        val pair2Ids = allOrderIdsByPair(pk, pair2)
        pair2Ids shouldBe pair2Orders.map(_.id()).reverse
      }
    }
  }

  property("Wrong events - OrderExecuted for non-existed orders") {
    val counter   = buy(WavesBtc, 100000, 0.0008, matcherFee = Some(2000L))
    val submitted = sell(WavesBtc, 100000, 0.0007, matcherFee = Some(1000L))

    oh.process(OrderExecuted(LimitOrder(submitted), LimitOrder(counter)))

    withClue(s"has no reserved assets, counter.senderPublicKey: ${counter.senderPublicKey}, counter.order.id=${counter.id()}") {
      openVolume(counter.senderPublicKey, WavesBtc.amountAsset) shouldBe 0L
      openVolume(counter.senderPublicKey, WavesBtc.priceAsset) shouldBe 0L
    }

    withClue(s"has no reserved assets, submitted.senderPublicKey: ${submitted.senderPublicKey}, submitted.order.id=${submitted.id()}") {
      openVolume(submitted.senderPublicKey, WavesBtc.amountAsset) shouldBe 0L
      openVolume(submitted.senderPublicKey, WavesBtc.priceAsset) shouldBe 0L
    }

    withClue("orders list of counter owner") {
      activeOrderIds(counter.senderPublicKey) shouldBe empty
      activeOrderIdsByPair(counter.senderPublicKey, WavesBtc) shouldBe empty
    }

    withClue("orders list of submitted owner") {
      activeOrderIds(submitted.senderPublicKey) shouldBe empty
      activeOrderIdsByPair(submitted.senderPublicKey, WavesBtc) shouldBe empty
    }
  }

  property("Idempotence - OrderAdded") {
    val ord = buy(WctBtc, 10000, 0.0007)

    val lo  = LimitOrder(ord)
    val add = OrderAdded(lo)
    oh.processAll(add, add)

    withClue("info") {
      orderStatus(ord.id()) shouldBe OrderStatus.Accepted
    }

    withClue("reserved assets") {
      openVolume(ord.senderPublicKey, WctBtc.amountAsset) shouldBe 0L
      openVolume(ord.senderPublicKey, WctBtc.priceAsset) shouldBe 7L
      openVolume(ord.senderPublicKey, Waves) shouldBe ord.matcherFee
    }

    withClue("orders list") {
      val expected = Seq(ord.id())

      activeOrderIds(ord.senderPublicKey) shouldBe expected
      allOrderIds(ord.senderPublicKey) shouldBe expected

      activeOrderIdsByPair(ord.senderPublicKey, WctBtc) shouldBe expected
      allOrderIdsByPair(ord.senderPublicKey, WctBtc) shouldBe expected
    }
  }

  property("Idempotence - OrderExecuted") {
    val counter   = buy(WavesBtc, 100000, 0.0008, matcherFee = Some(2000L))
    val submitted = sell(WavesBtc, 100000, 0.0007, matcherFee = Some(1000L))

    oh.process(OrderAdded(LimitOrder(counter)))

    val exec = OrderExecuted(LimitOrder(submitted), LimitOrder(counter))
    oh.processAll(exec, exec)

    withClue("executed exactly") {
      exec.executedAmount shouldBe counter.amount
      orderStatus(counter.id()) shouldBe OrderStatus.Filled(exec.executedAmount)
      orderStatus(submitted.id()) shouldBe OrderStatus.Filled(exec.executedAmount)
    }

    withClue(s"has no reserved assets, counter.senderPublicKey: ${counter.senderPublicKey}, counter.order.id=${counter.idStr()}") {
      openVolume(counter.senderPublicKey, WavesBtc.amountAsset) shouldBe 0L
      openVolume(counter.senderPublicKey, WavesBtc.priceAsset) shouldBe 0L
    }

    withClue(s"has no reserved assets, submitted.senderPublicKey: ${submitted.senderPublicKey}, submitted.order.id=${submitted.idStr()}") {
      openVolume(submitted.senderPublicKey, WavesBtc.amountAsset) shouldBe 0L
      openVolume(submitted.senderPublicKey, WavesBtc.priceAsset) shouldBe 0L
    }

    withClue("orders list of counter owner") {
      activeOrderIds(counter.senderPublicKey) shouldBe empty
      allOrderIds(counter.senderPublicKey) shouldBe Seq(counter.id())

      activeOrderIdsByPair(counter.senderPublicKey, WavesBtc) shouldBe empty
      allOrderIdsByPair(counter.senderPublicKey, WavesBtc) shouldBe Seq(counter.id())
    }

    withClue("orders list of submitted owner") {
      activeOrderIds(submitted.senderPublicKey) shouldBe empty
      allOrderIds(submitted.senderPublicKey) shouldBe Seq(submitted.id())

      activeOrderIdsByPair(submitted.senderPublicKey, WavesBtc) shouldBe empty
      allOrderIdsByPair(submitted.senderPublicKey, WavesBtc) shouldBe Seq(submitted.id())
    }
  }

  property("Idempotence - OrderCancelled") {
    val ord1   = buy(WctBtc, 100000000, 0.0008, matcherFee = Some(300000L))
    val cancel = OrderCanceled(LimitOrder(ord1), unmatchable = false)
    oh.processAll(OrderAdded(LimitOrder(ord1)), cancel, cancel)

    orderStatus(ord1.id()) shouldBe OrderStatus.Cancelled(0)

    openVolume(ord1.senderPublicKey, WctBtc.amountAsset) shouldBe 0L
    openVolume(ord1.senderPublicKey, WctBtc.priceAsset) shouldBe 0L

    withClue("orders list") {
      val addr = ord1.senderPublicKey.toAddress

      activeOrderIds(addr) shouldBe empty
      allOrderIds(addr) shouldBe Seq(ord1.id())

      activeOrderIdsByPair(addr, WctBtc) shouldBe empty
      allOrderIdsByPair(addr, WctBtc) shouldBe Seq(ord1.id())
    }
  }
}

private object OrderHistoryBalanceSpecification {
  val MaxElements: Int             = 100
  implicit val askTimeout: Timeout = 5.seconds

  private def askAddressActor[A: ClassTag](ref: ActorRef, msg: Any) =
    Await.result((ref ? msg).mapTo[A], 5.seconds)

  private implicit class AddressActorExt(val ref: ActorRef) extends AnyVal {
    def orderIds(assetPair: Option[AssetPair], activeOnly: Boolean): Seq[Order.Id] =
      askAddressActor[Seq[(ByteStr, OrderInfo[OrderStatus])]](ref, AddressActor.GetOrders(assetPair, activeOnly)).map(_._1)

    def activeOrderIds: Seq[Order.Id] = orderIds(None, true)

    def allOrderIds: Seq[Order.Id] = orderIds(None, false)

    def activeOrderIdsByPair(pair: AssetPair): Seq[Order.Id] = orderIds(Some(pair), true)

    def allOrderIdsByPair(pair: AssetPair): Seq[Order.Id] = orderIds(Some(pair), false)

    def openVolume(asset: Asset): Long =
      askAddressActor[Map[Asset, Long]](ref, AddressActor.GetReservedBalance).getOrElse(asset, 0L)

    def orderStatus(orderId: ByteStr): OrderStatus =
      askAddressActor[OrderStatus](ref, AddressActor.GetOrderStatus(orderId))
  }

  private implicit class OrderExecutedExt(val oe: OrderExecuted.type) extends AnyVal {
    def apply(submitted: LimitOrder, counter: LimitOrder): OrderExecuted = OrderExecuted(submitted, counter, submitted.order.timestamp)
  }
}

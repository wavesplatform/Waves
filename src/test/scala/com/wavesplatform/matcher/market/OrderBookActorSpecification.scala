package com.wavesplatform.matcher.market

import java.util.concurrent.ConcurrentHashMap

import akka.actor.{ActorRef, Props}
import akka.testkit.{ImplicitSender, TestProbe}
import com.wavesplatform.NTPTime
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.matcher.api.AlreadyProcessed
import com.wavesplatform.matcher.fixtures.RestartableActor
import com.wavesplatform.matcher.fixtures.RestartableActor.RestartActor
import com.wavesplatform.matcher.market.MatcherActor.SaveSnapshot
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.model.Events.OrderAdded
import com.wavesplatform.matcher.model._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.OrderOps._
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.utils.EmptyBlockchain
import org.scalamock.scalatest.PathMockFactory

import scala.concurrent.duration._
import scala.util.Random

class OrderBookActorSpecification extends MatcherSpec("OrderBookActor") with NTPTime with ImplicitSender with MatcherTestData with PathMockFactory {

  private val txFactory = new ExchangeTransactionCreator(EmptyBlockchain, MatcherAccount, matcherSettings.orderFee).createTransaction _
  private val obc       = new ConcurrentHashMap[AssetPair, OrderBook.AggregatedSnapshot]
  private val md        = new ConcurrentHashMap[AssetPair, MarketStatus]

  private def update(ap: AssetPair)(snapshot: OrderBook.AggregatedSnapshot): Unit = obc.put(ap, snapshot)

  private def obcTest(f: (AssetPair, ActorRef, TestProbe) => Unit): Unit = {
    obc.clear()
    md.clear()
    val b = ByteStr(new Array[Byte](32))
    Random.nextBytes(b.arr)

    val tp   = TestProbe()
    val pair = AssetPair(IssuedAsset(b), Waves)
    val actor = system.actorOf(
      Props(
        new OrderBookActor(
          tp.ref,
          tp.ref,
          pair,
          update(pair),
          p => Option(md.get(p)),
          _ => {},
          txFactory,
          ntpTime
        ) with RestartableActor))

    tp.expectMsg(OrderBookSnapshotUpdated(pair, -1))

    f(pair, actor, tp)
  }

  "OrderBookActor" should {

    "place buy and sell order to the order book and preserve it after restart" in obcTest { (pair, orderBook, tp) =>
      val ord1 = buy(pair, 10 * Order.PriceConstant, 100)
      val ord2 = sell(pair, 15 * Order.PriceConstant, 150)

      orderBook ! wrap(ord1)
      orderBook ! wrap(ord2)
      tp.receiveN(2)

      orderBook ! SaveSnapshot(Long.MaxValue)
      tp.expectMsgType[OrderBookSnapshotUpdated]
      orderBook ! RestartActor

      tp.receiveN(2) shouldEqual Seq(ord2, ord1).map(o => OrderAdded(LimitOrder(o)))
    }

    "execute partial market orders and preserve remaining after restart" in obcTest { (pair, actor, tp) =>
      val ord1 = buy(pair, 10 * Order.PriceConstant, 100)
      val ord2 = sell(pair, 15 * Order.PriceConstant, 100)

      actor ! wrap(ord1)
      actor ! wrap(ord2)

      tp.receiveN(3)

      actor ! SaveSnapshot(Long.MaxValue)
      tp.expectMsgType[OrderBookSnapshotUpdated]
      actor ! RestartActor

      tp.expectMsg(
        OrderAdded(
          SellLimitOrder(
            ord2.amount - ord1.amount,
            ord2.matcherFee - LimitOrder.partialFee(ord2.matcherFee, ord2.amount, ord1.amount),
            ord2
          )))
    }

    "execute one order fully and other partially and restore after restart" in obcTest { (pair, actor, tp) =>
      val ord1 = buy(pair, 10 * Order.PriceConstant, 100)
      val ord2 = buy(pair, 5 * Order.PriceConstant, 100)
      val ord3 = sell(pair, 12 * Order.PriceConstant, 100)

      actor ! wrap(ord1)
      actor ! wrap(ord2)
      actor ! wrap(ord3)
      tp.receiveN(4)

      actor ! SaveSnapshot(Long.MaxValue)
      tp.expectMsgType[OrderBookSnapshotUpdated]
      actor ! RestartActor

      val restAmount = ord1.amount + ord2.amount - ord3.amount
      tp.expectMsg(
        OrderAdded(
          BuyLimitOrder(
            restAmount,
            ord2.matcherFee - LimitOrder.partialFee(ord2.matcherFee, ord2.amount, ord2.amount - restAmount),
            ord2
          )))
    }

    "match multiple best orders at once and restore after restart" in obcTest { (pair, actor, tp) =>
      val ord1 = sell(pair, 10 * Order.PriceConstant, 100)
      val ord2 = sell(pair, 5 * Order.PriceConstant, 100)
      val ord3 = sell(pair, 5 * Order.PriceConstant, 90)
      val ord4 = buy(pair, 19 * Order.PriceConstant, 100)

      actor ! wrap(ord1)
      actor ! wrap(ord2)
      actor ! wrap(ord3)
      actor ! wrap(ord4)
      tp.receiveN(6)

      actor ! SaveSnapshot(Long.MaxValue)
      tp.expectMsgType[OrderBookSnapshotUpdated]
      actor ! RestartActor

      val restAmount = ord1.amount + ord2.amount + ord3.amount - ord4.amount
      tp.expectMsg(
        OrderAdded(
          SellLimitOrder(
            restAmount,
            ord2.matcherFee - LimitOrder.partialFee(ord2.matcherFee, ord2.amount, ord2.amount - restAmount),
            ord2
          )))
    }

    "place orders and restart without waiting for response" in obcTest { (pair, actor, tp) =>
      val ord1 = sell(pair, 10 * Order.PriceConstant, 100)
      val ts   = System.currentTimeMillis()

      (1 to 100).foreach({ i =>
        actor ! wrap(ord1.updateTimestamp(ts + i))
      })

      within(10.seconds) {
        tp.receiveN(100)
      }

      actor ! SaveSnapshot(Long.MaxValue)
      tp.expectMsgType[OrderBookSnapshotUpdated]
      actor ! RestartActor

      within(10.seconds) {
        tp.receiveN(100)
      }
    }

    "ignore outdated requests" in obcTest { (pair, actor, tp) =>
      (1 to 10).foreach { i =>
        actor ! wrap(i, buy(pair, 100000000, 0.00041))
      }
      tp.receiveN(10)

      (1 to 10).foreach { i =>
        actor ! wrap(i, buy(pair, 100000000, 0.00041))
      }
      all(receiveN(10)) shouldBe AlreadyProcessed
    }

    "respond on SaveSnapshotCommand" in obcTest { (pair, actor, tp) =>
      (1 to 10).foreach { i =>
        actor ! wrap(i, buy(pair, 100000000, 0.00041))
      }
      tp.receiveN(10)

      actor ! SaveSnapshot(10)
      tp.expectMsg(OrderBookSnapshotUpdated(pair, 10))

      (11 to 20).foreach { i =>
        actor ! wrap(i, buy(pair, 100000000, 0.00041))
      }
      tp.receiveN(10)

      actor ! SaveSnapshot(20)
      tp.expectMsg(OrderBookSnapshotUpdated(pair, 20))
    }

    "don't do a snapshot if there is no changes" in obcTest { (pair, actor, tp) =>
      (1 to 10).foreach { i =>
        actor ! wrap(i, buy(pair, 100000000, 0.00041))
      }
      tp.receiveN(10)

      actor ! SaveSnapshot(10)
      actor ! SaveSnapshot(10)
      tp.expectMsgType[OrderBookSnapshotUpdated]
      tp.expectNoMessage(200.millis)
    }

    "restore its state at start" in obcTest { (pair, actor, tp) =>
      (1 to 10).foreach { i =>
        actor ! wrap(i, buy(pair, 100000000, 0.00041))
      }
      tp.receiveN(10)

      actor ! SaveSnapshot(10)
      tp.expectMsgType[OrderBookSnapshotUpdated]
    }
  }
}

package com.wavesplatform.matcher.market

import java.util.concurrent.ConcurrentHashMap

import akka.actor.{ActorRef, Props}
import akka.testkit.{ImplicitSender, TestProbe}
import com.wavesplatform.NTPTime
import com.wavesplatform.OrderOps._
import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.matcher.api.{AlreadyProcessed, OrderAccepted}
import com.wavesplatform.matcher.fixtures.RestartableActor.RestartActor
import com.wavesplatform.matcher.market.MatcherActor.SaveSnapshot
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.model.Events.OrderAdded
import com.wavesplatform.matcher.model._
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.utils.EmptyBlockchain
import org.scalamock.scalatest.PathMockFactory

import scala.concurrent.duration._
import scala.util.Random

class OrderBookActorSpecification extends MatcherSpec("OrderBookActor") with NTPTime with ImplicitSender with MatcherTestData with PathMockFactory {

  private val txFactory = new ExchangeTransactionCreator(EmptyBlockchain, MatcherAccount, matcherSettings).createTransaction _
  private val obc       = new ConcurrentHashMap[AssetPair, OrderBook.Snapshot]
  private val md        = new ConcurrentHashMap[AssetPair, MarketStatus]

  private def update(ap: AssetPair)(snapshot: OrderBook.Snapshot): Unit = obc.put(ap, snapshot)

  private def obcTest(f: (AssetPair, ActorRef) => Unit): Unit = {
    obc.clear()
    md.clear()
    val b = ByteStr(new Array[Byte](32))
    Random.nextBytes(b.arr)

    val pair = AssetPair(Some(b), None)
    val actor = system.actorOf(
      Props(
        new OrderBookActor(
          testActor,
          TestProbe().ref,
          pair,
          update(pair),
          p => Option(md.get(p)),
          _ => {},
          txFactory,
          ntpTime
        )))

    expectMsg(OrderBookSnapshotUpdated(pair, -1))

    f(pair, actor)
  }

  "OrderBookActor" should {

    "place buy and sell order to the order book and preserve it after restart" in obcTest { (pair, actor) =>
      val ord1 = buy(pair, 10 * Order.PriceConstant, 100)
      val ord2 = sell(pair, 15 * Order.PriceConstant, 150)

      actor ! wrap(ord1)
      actor ! wrap(ord2)
      receiveN(2)

      actor ! RestartActor

      receiveN(2) shouldEqual Seq(ord2, ord1).map(o => OrderAdded(LimitOrder(o)))
    }

    "execute partial market orders and preserve remaining after restart" in obcTest { (pair, actor) =>
      val ord1 = buy(pair, 10 * Order.PriceConstant, 100)
      val ord2 = sell(pair, 15 * Order.PriceConstant, 100)

      actor ! wrap(ord1)
      expectMsgType[OrderAccepted]
      actor ! wrap(ord2)
      expectMsgType[OrderAccepted]

      actor ! RestartActor

      expectMsg(
        OrderAdded(
          SellLimitOrder(
            ord2.amount - ord1.amount,
            ord2.matcherFee - LimitOrder.getPartialFee(ord2.matcherFee, ord2.amount, ord1.amount),
            ord2
          )))

      expectNoMessage()
    }

    "execute one order fully and other partially and restore after restart" in obcTest { (pair, actor) =>
      val ord1 = buy(pair, 10 * Order.PriceConstant, 100)
      val ord2 = buy(pair, 5 * Order.PriceConstant, 100)
      val ord3 = sell(pair, 12 * Order.PriceConstant, 100)

      actor ! wrap(ord1)
      actor ! wrap(ord2)
      actor ! wrap(ord3)
      receiveN(3)

      actor ! RestartActor

      val restAmount = ord1.amount + ord2.amount - ord3.amount
      expectMsg(
        OrderAdded(
          BuyLimitOrder(
            restAmount,
            ord2.matcherFee - LimitOrder.getPartialFee(ord2.matcherFee, ord2.amount, ord2.amount - restAmount),
            ord2
          )))

      expectNoMessage()
    }

    "match multiple best orders at once and restore after restart" in obcTest { (pair, actor) =>
      val ord1 = sell(pair, 10 * Order.PriceConstant, 100)
      val ord2 = sell(pair, 5 * Order.PriceConstant, 100)
      val ord3 = sell(pair, 5 * Order.PriceConstant, 90)
      val ord4 = buy(pair, 19 * Order.PriceConstant, 100)

      actor ! wrap(ord1)
      actor ! wrap(ord2)
      actor ! wrap(ord3)
      actor ! wrap(ord4)
      receiveN(4)

      actor ! RestartActor

      val restAmount = ord1.amount + ord2.amount + ord3.amount - ord4.amount
      expectMsg(
        OrderAdded(
          SellLimitOrder(
            restAmount,
            ord2.matcherFee - LimitOrder.getPartialFee(ord2.matcherFee, ord2.amount, ord2.amount - restAmount),
            ord2
          )))

      expectNoMessage()
    }

    "place orders and restart without waiting for response" in obcTest { (pair, actor) =>
      val ord1 = sell(pair, 10 * Order.PriceConstant, 100)
      val ts   = System.currentTimeMillis()

      (1 to 100).foreach({ i =>
        actor ! wrap(ord1.updateTimestamp(ts + i))
      })

      within(10.seconds) {
        receiveN(100)
      }

      actor ! RestartActor

      within(10.seconds) {
        receiveN(100)
      }
    }

    "order matched with invalid order should keep matching with others, invalid is removed" in obcTest { (pair, _) =>
      val ord1       = buy(pair, 20 * Order.PriceConstant, 100)
      val invalidOrd = buy(pair, 1000 * Order.PriceConstant, 5000)
      val ord2       = sell(pair, 10 * Order.PriceConstant, 100)

      val actor = system.actorOf(
        Props(new OrderBookActor(
          TestProbe().ref,
          TestProbe().ref,
          pair,
          update(pair),
          m => md.put(pair, m),
          _ => {},
          (submitted, counter, ts) => {
            if (submitted.order == invalidOrd || counter.order == invalidOrd)
              Left(ValidationError.OrderValidationError(invalidOrd, "It's an invalid!"))
            else txFactory(submitted, counter, ts)
          },
          ntpTime
        )))

      actor ! wrap(ord1)
      expectMsg(OrderAccepted(ord1))
      actor ! wrap(invalidOrd)
      expectMsg(OrderAccepted(invalidOrd))
      actor ! wrap(ord2)
      expectMsg(OrderAccepted(ord2))

      actor ! RestartActor

      val restAmount = ord1.amount - ord2.amount
      expectMsg(
        OrderAdded(
          BuyLimitOrder(
            restAmount,
            ord1.matcherFee - LimitOrder.getPartialFee(ord1.matcherFee, ord1.amount, restAmount),
            ord1
          )))

      expectNoMessage()
    }

    "ignore outdated requests" in obcTest { (pair, actor) =>
      (1 to 10).foreach { i =>
        actor ! wrap(i, buy(pair, 100000000, 0.00041))
      }
      receiveN(10)

      (1 to 10).foreach { i =>
        actor ! wrap(i, buy(pair, 100000000, 0.00041))
      }
      all(receiveN(10)) shouldBe AlreadyProcessed
    }

    "respond on SaveSnapshotCommand" in obcTest { (pair, actor) =>
      (1 to 10).foreach { i =>
        actor ! wrap(i, buy(pair, 100000000, 0.00041))
      }
      receiveN(10)

      actor ! SaveSnapshot(10)
      expectMsg(OrderBookSnapshotUpdated(pair, 10))

      (11 to 20).foreach { i =>
        actor ! wrap(i, buy(pair, 100000000, 0.00041))
      }
      receiveN(10)

      actor ! SaveSnapshot(20)
      expectMsg(OrderBookSnapshotUpdated(pair, 20))
    }

    "don't do a snapshot if there is no changes" in obcTest { (pair, actor) =>
      (1 to 10).foreach { i =>
        actor ! wrap(i, buy(pair, 100000000, 0.00041))
      }
      receiveN(10)

      actor ! SaveSnapshot(10)
      actor ! SaveSnapshot(10)
      expectMsgType[OrderBookSnapshotUpdated]
      expectNoMessage(200.millis)
    }

    "restore its state at start" in obcTest { (pair, actor) =>
      (1 to 10).foreach { i =>
        actor ! wrap(i, buy(pair, 100000000, 0.00041))
      }
      receiveN(10)

      actor ! SaveSnapshot(10)
      expectMsgType[OrderBookSnapshotUpdated]
    }
  }
}

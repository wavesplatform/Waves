package com.wavesplatform.matcher.market

import java.util.concurrent.ConcurrentHashMap

import akka.actor.{ActorRef, Props}
import akka.testkit.{ImplicitSender, TestProbe}
import com.wavesplatform.NTPTime
import com.wavesplatform.OrderOps._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.matcher.api.{OrderAccepted, OrderCanceled}
import com.wavesplatform.matcher.fixtures.RestartableActor
import com.wavesplatform.matcher.fixtures.RestartableActor.RestartActor
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.model._
import com.wavesplatform.settings.Constants
import com.wavesplatform.state.Diff
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order}
import com.wavesplatform.utils.EmptyBlockchain
import com.wavesplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup
import org.scalamock.scalatest.PathMockFactory

import scala.concurrent.duration._
import scala.util.Random

class OrderBookActorSpecification extends MatcherSpec("OrderBookActor") with NTPTime with ImplicitSender with MatcherTestData with PathMockFactory {

  private val txFactory = new ExchangeTransactionCreator(EmptyBlockchain, MatcherAccount, matcherSettings, ntpTime).createTransaction _
  private val obc       = new ConcurrentHashMap[AssetPair, OrderBook]
  private val md        = new ConcurrentHashMap[AssetPair, MarketStatus]

  private def update(ap: AssetPair)(snapshot: OrderBook): Unit = obc.put(ap, snapshot)

  private def getOrders(actor: ActorRef) = {
    actor ! GetOrdersRequest
    receiveN(1).head.asInstanceOf[GetOrdersResponse].orders
  }

  private def obcTest(f: (AssetPair, ActorRef) => Unit): Unit = {
    obc.clear()
    md.clear()
    val b = ByteStr(new Array[Byte](32))
    Random.nextBytes(b.arr)

    val pair = AssetPair(Some(b), None)

    val utx = stub[UtxPool]
    (utx.putIfNew _).when(*).onCall((_: Transaction) => Right((true, Diff.empty)))
    val allChannels = stub[ChannelGroup]
    val actor = system.actorOf(
      Props(
        new OrderBookActor(TestProbe().ref, pair, update(pair), p => Option(md.get(p)), utx, allChannels, matcherSettings, txFactory, ntpTime)
        with RestartableActor))

    f(pair, actor)
  }

  "OrderBookActor" should {

    "place buy orders" in obcTest { (pair, actor) =>
      val ord1 = buy(pair, 1583290045643L, 34118)
      val ord2 = buy(pair, 170484969L, 34120)
      val ord3 = buy(pair, 44521418496L, 34000)

      actor ! ord1
      expectMsg(OrderAccepted(ord1))
      actor ! ord2
      expectMsg(OrderAccepted(ord2))
      actor ! ord3
      expectMsg(OrderAccepted(ord3))

      actor ! GetOrdersRequest
      expectMsg(GetOrdersResponse(Seq(ord2, ord1, ord3).map(LimitOrder(_))))
    }

    "place sell orders" in obcTest { (pair, actor) =>
      val ord1 = sell(pair, 1583290045643L, 34110)
      val ord2 = sell(pair, 170484969L, 34220)
      val ord3 = sell(pair, 44521418496L, 34000)

      actor ! ord1
      expectMsg(OrderAccepted(ord1))
      actor ! ord2
      expectMsg(OrderAccepted(ord2))
      actor ! ord3
      expectMsg(OrderAccepted(ord3))

      actor ! GetOrdersRequest
      expectMsg(GetOrdersResponse(Seq(ord3, ord1, ord2).map(LimitOrder(_))))
    }

    "sell market" in obcTest { (pair, actor) =>
      val ord1 = buy(pair, 10 * Order.PriceConstant, 100)
      val ord2 = buy(pair, 10 * Order.PriceConstant, 105)

      actor ! ord1
      actor ! ord2
      receiveN(2)
      actor ! GetOrdersRequest
      expectMsg(GetOrdersResponse(Seq(BuyLimitOrder(ord2.amount, ord2.matcherFee, ord2), BuyLimitOrder(ord1.amount, ord1.matcherFee, ord1))))

      val ord3 = sell(pair, 10 * Order.PriceConstant, 100)
      actor ! ord3
      expectMsg(OrderAccepted(ord3))

      actor ! GetOrdersRequest
      expectMsg(GetOrdersResponse(Seq(BuyLimitOrder(ord1.amount, ord1.matcherFee, ord1))))
    }

    "place buy and sell order to the order book and preserve it after restart" in obcTest { (pair, actor) =>
      val ord1 = buy(pair, 10 * Order.PriceConstant, 100)
      val ord2 = sell(pair, 15 * Order.PriceConstant, 150)

      actor ! ord1
      actor ! ord2
      receiveN(2)

      actor ! RestartActor
      actor ! GetOrdersRequest

      expectMsg(GetOrdersResponse(Seq(ord2, ord1).map(LimitOrder(_))))
    }

    "execute partial market orders and preserve remaining after restart" in obcTest { (pair, actor) =>
      val ord1 = buy(pair, 10 * Order.PriceConstant, 100)
      val ord2 = sell(pair, 15 * Order.PriceConstant, 100)

      actor ! ord1
      expectMsgType[OrderAccepted]
      actor ! ord2
      expectMsgType[OrderAccepted]

      actor ! RestartActor
      actor ! GetOrdersRequest

      expectMsg(
        GetOrdersResponse(
          Seq(
            SellLimitOrder(
              ord2.amount - ord1.amount,
              ord2.matcherFee - LimitOrder.getPartialFee(ord2.matcherFee, ord2.amount, ord1.amount),
              ord2
            ))))
    }

    "execute one order fully and other partially and restore after restart" in obcTest { (pair, actor) =>
      val ord1 = buy(pair, 10 * Order.PriceConstant, 100)
      val ord2 = buy(pair, 5 * Order.PriceConstant, 100)
      val ord3 = sell(pair, 12 * Order.PriceConstant, 100)

      actor ! ord1
      actor ! ord2
      actor ! ord3
      receiveN(3)

      actor ! RestartActor

      actor ! GetBidOrdersRequest
      val restAmount = ord1.amount + ord2.amount - ord3.amount
      expectMsg(
        GetOrdersResponse(
          Seq(
            BuyLimitOrder(
              restAmount,
              ord2.matcherFee - LimitOrder.getPartialFee(ord2.matcherFee, ord2.amount, ord2.amount - restAmount),
              ord2
            ))))

      actor ! GetAskOrdersRequest
      expectMsg(GetOrdersResponse(Seq.empty))
    }

    "match multiple best orders at once and restore after restart" in obcTest { (pair, actor) =>
      val ord1 = sell(pair, 10 * Order.PriceConstant, 100)
      val ord2 = sell(pair, 5 * Order.PriceConstant, 100)
      val ord3 = sell(pair, 5 * Order.PriceConstant, 90)
      val ord4 = buy(pair, 19 * Order.PriceConstant, 100)

      actor ! ord1
      actor ! ord2
      actor ! ord3
      actor ! ord4
      receiveN(4)

      actor ! RestartActor

      actor ! GetBidOrdersRequest
      expectMsg(GetOrdersResponse(Seq.empty))

      actor ! GetAskOrdersRequest
      val restAmount = ord1.amount + ord2.amount + ord3.amount - ord4.amount
      expectMsg(
        GetOrdersResponse(
          Seq(
            SellLimitOrder(
              restAmount,
              ord2.matcherFee - LimitOrder.getPartialFee(ord2.matcherFee, ord2.amount, ord2.amount - restAmount),
              ord2
            ))))

    }

    "execute orders at different price levels" in obcTest { (pair, actor) =>
      val ord1 = sell(pair, 10 * Order.PriceConstant, 100)
      val ord2 = sell(pair, 5 * Order.PriceConstant, 110)
      val ord3 = sell(pair, 10 * Order.PriceConstant, 110)
      val ord4 = buy(pair, 22 * Order.PriceConstant, 115)

      actor ! ord1
      actor ! ord2
      actor ! ord3
      actor ! ord4
      receiveN(4)

      actor ! GetBidOrdersRequest
      expectMsg(GetOrdersResponse(Seq.empty))

      actor ! GetAskOrdersRequest
      val restAmount = ord1.amount + ord2.amount + ord3.amount - ord4.amount
      expectMsg(
        GetOrdersResponse(
          Seq(
            SellLimitOrder(
              restAmount,
              ord3.matcherFee - LimitOrder.getPartialFee(ord3.matcherFee, ord3.amount, ord3.amount - restAmount),
              ord3
            ))))
    }

    "place orders and restart without waiting for response" in obcTest { (pair, actor) =>
      val ord1 = sell(pair, 10 * Order.PriceConstant, 100)
      val ts   = System.currentTimeMillis()

      (1 to 100).foreach({ i =>
        actor ! ord1.updateTimestamp(ts + i)
      })

      within(10.seconds) {
        receiveN(100)
      }

      actor ! RestartActor

      within(10.seconds) {
        actor ! GetOrdersRequest
        expectMsgType[GetOrdersResponse].orders should have size 100
      }
    }

    "order matched with invalid order should keep matching with others, invalid is removed" in obcTest { (pair, _) =>
      val ord1       = buy(pair, 20 * Order.PriceConstant, 100)
      val invalidOrd = buy(pair, 1000 * Order.PriceConstant, 5000)
      val ord2       = sell(pair, 10 * Order.PriceConstant, 100)

      val pool = stub[UtxPool]
      (pool.putIfNew _).when(*).onCall { tx: Transaction =>
        tx match {
          case om: ExchangeTransaction if om.buyOrder == invalidOrd => Left(ValidationError.GenericError("test"))
          case _: Transaction                                       => Right((true, Diff.empty))
        }
      }
      val allChannels = stub[ChannelGroup]
      val actor = system.actorOf(
        Props(new OrderBookActor(TestProbe().ref, pair, update(pair), m => md.put(pair, m), pool, allChannels, matcherSettings, txFactory, ntpTime)
        with RestartableActor))

      actor ! ord1
      expectMsg(OrderAccepted(ord1))
      actor ! invalidOrd
      expectMsg(OrderAccepted(invalidOrd))
      actor ! ord2
      expectMsg(OrderAccepted(ord2))

      actor ! RestartActor

      actor ! GetBidOrdersRequest
      val restAmount = ord1.amount - ord2.amount
      expectMsg(
        GetOrdersResponse(
          Seq(
            BuyLimitOrder(
              restAmount,
              ord1.matcherFee - LimitOrder.getPartialFee(ord1.matcherFee, ord1.amount, restAmount),
              ord1
            ))))

      actor ! GetAskOrdersRequest
      expectMsg(GetOrdersResponse(Seq.empty))

    }

    "partially execute order with small remaining part" in obcTest { (pair, actor) =>
      val ord1 = sell(pair, 200000000, 0.00041)
      val ord2 = sell(pair, 100000000, 0.0004)
      val ord3 = buy(pair, 100000001, 0.00045)

      actor ! ord1
      actor ! ord2
      actor ! ord3
      receiveN(3)

      actor ! GetAskOrdersRequest
      expectMsg(GetOrdersResponse(Seq(SellLimitOrder(ord1.amount, ord1.matcherFee, ord1))))

    }

    "partially execute order with zero fee remaining part" in obcTest { (pair, actor) =>
      val ord1 = sell(pair, 1500 * Constants.UnitsInWave, 0.0006999)
      val ord2 = sell(pair, 3075248828L, 0.00067634)
      val ord3 = buy(pair, 3075363900L, 0.00073697)

      actor ! ord1
      actor ! ord2
      actor ! ord3
      receiveN(3)

      actor ! GetAskOrdersRequest
      val corrected1 = Order.correctAmount(ord2.amount, ord2.price)
      val leftovers1 = ord3.amount - corrected1
      val corrected2 = Order.correctAmount(leftovers1, ord1.price)
      val restAmount = ord1.amount - corrected2
      // See OrderExecuted.submittedRemainingFee
      val restFee = ord1.matcherFee - LimitOrder.getPartialFee(ord1.matcherFee, ord1.amount, corrected2)
      expectMsg(GetOrdersResponse(Seq(SellLimitOrder(restAmount, restFee, ord1))))
    }

    "partially execute order with price > 1 and zero fee remaining part " in obcTest { (_, actor) =>
      val pair = AssetPair(Some(ByteStr("BTC".getBytes)), Some(ByteStr("USD".getBytes)))
      val ord1 = sell(pair, (0.1 * Constants.UnitsInWave).toLong, 1850)
      val ord2 = sell(pair, (0.01 * Constants.UnitsInWave).toLong, 1840)
      val ord3 = buy(pair, (0.0100001 * Constants.UnitsInWave).toLong, 2000)

      actor ! ord1
      actor ! ord2
      actor ! ord3
      receiveN(3)

      actor ! GetAskOrdersRequest
      val restAmount = ord1.amount - (ord3.amount - ord2.amount)
      val restFee    = ord1.matcherFee - LimitOrder.getPartialFee(ord1.matcherFee, ord1.amount, ord3.amount - ord2.amount)
      expectMsg(GetOrdersResponse(Seq(SellLimitOrder(restAmount, restFee, ord1))))
    }

    "buy small amount of pricey asset" in obcTest { (_, actor) =>
      val p = AssetPair(Some(ByteStr("WAVES".getBytes)), Some(ByteStr("USD".getBytes)))
      val b = rawBuy(p, 700000L, 280)
      val s = rawSell(p, 30000000000L, 280)
      actor ! s
      actor ! b
      receiveN(2)

      actor ! GetAskOrdersRequest
      val restSAmount = Order.correctAmount(700000L, 280)
      val restAmount  = 30000000000L - restSAmount
      val restFee     = s.matcherFee - LimitOrder.getPartialFee(s.matcherFee, s.amount, restSAmount)
      expectMsg(GetOrdersResponse(Seq(SellLimitOrder(restAmount, restFee, s))))

      actor ! GetBidOrdersRequest
      expectMsg(GetOrdersResponse(Seq.empty))
    }

    "cancel expired orders after OrderCleanup command" in obcTest { (pair, actor) =>
      val ts     = ntpTime.correctedTime()
      val amount = 1
      val price  = 34118

      val expiredOrder = buy(pair, amount, price).updateExpiration(ts)
      actor ! expiredOrder
      receiveN(1)
      getOrders(actor) shouldEqual Seq(BuyLimitOrder(amount, expiredOrder.matcherFee, expiredOrder))
      actor ! OrderCleanup
      expectMsg(OrderCanceled(expiredOrder.id()))
      getOrders(actor).size should be(0)
    }

    "preserve valid orders after OrderCleanup command" in obcTest { (pair, actor) =>
      val amount = 1
      val price  = 34118

      val order          = buy(pair, amount, price)
      val expectedOrders = Seq(BuyLimitOrder(amount, order.matcherFee, order))

      actor ! order
      receiveN(1)
      getOrders(actor) shouldEqual expectedOrders
      actor ! OrderCleanup
      getOrders(actor) shouldEqual expectedOrders
    }
  }
}

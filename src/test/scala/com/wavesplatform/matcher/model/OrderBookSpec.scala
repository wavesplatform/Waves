package com.wavesplatform.matcher.model

import java.nio.ByteBuffer

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.matcher.model.OrderBook.{LastTrade, SideSnapshot, Snapshot}
import com.wavesplatform.settings.Constants
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.{NTPTime, NoShrink}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}

import scala.collection.mutable

class OrderBookSpec extends FreeSpec with PropertyChecks with Matchers with MatcherTestData with NTPTime with NoShrink {
  val pair: AssetPair = AssetPair(None, mkAssetId("BTC"))

  "place buy orders with different prices" in {
    val ord1 = buy(pair, 1583290045643L, 34118)
    val ord2 = buy(pair, 170484969L, 34120)
    val ord3 = buy(pair, 44521418496L, 34000)

    val ob = OrderBook.empty
    ob.add(ord1, ntpNow)
    ob.add(ord2, ntpNow)
    ob.add(ord3, ntpNow)

    ob.allOrders.toSeq shouldEqual Seq(LimitOrder(ord2), LimitOrder(ord1), LimitOrder(ord3))
  }

  "place several buy orders at the same price" in {}

  "place sell orders" in {
    val ord1 = sell(pair, 1583290045643L, 34110)
    val ord2 = sell(pair, 170484969L, 34220)
    val ord3 = sell(pair, 44521418496L, 34000)

    Seq(ord3, ord1, ord2).map(LimitOrder(_))
  }

  "place several sell orders at the same price" in {}

  "sell market" in {
    val ord1 = buy(pair, 10 * Order.PriceConstant, 100)
    val ord2 = buy(pair, 10 * Order.PriceConstant, 105)

    val ob = OrderBook.empty
    ob.add(ord1, ntpNow)
    ob.add(ord2, ntpNow)

    ob.allOrders shouldEqual Seq(BuyLimitOrder(ord2.amount, ord2.matcherFee, ord2), BuyLimitOrder(ord1.amount, ord1.matcherFee, ord1))

    val ord3 = sell(pair, 10 * Order.PriceConstant, 100)
    ob.add(ord3, ntpNow)

    ob.allOrders shouldEqual Seq(BuyLimitOrder(ord1.amount, ord1.matcherFee, ord1))
  }

  "execute orders at different price levels" in {
    val ord1 = sell(pair, 10 * Order.PriceConstant, 100)
    val ord2 = sell(pair, 5 * Order.PriceConstant, 110)
    val ord3 = sell(pair, 10 * Order.PriceConstant, 110)
    val ord4 = buy(pair, 22 * Order.PriceConstant, 115)

    val ob = OrderBook.empty
    ob.add(ord1, ntpNow)
    ob.add(ord2, ntpNow)
    ob.add(ord3, ntpNow)
    ob.add(ord4, ntpNow)

    val restAmount = ord1.amount + ord2.amount + ord3.amount - ord4.amount

    ob.allOrders shouldEqual Seq(
      SellLimitOrder(
        restAmount,
        ord3.matcherFee - LimitOrder.partialFee(ord3.matcherFee, ord3.amount, ord3.amount - restAmount),
        ord3
      ))
  }

  "partially execute order with small remaining part" in {
    val ord1 = sell(pair, 200000000, 0.00041)
    val ord2 = sell(pair, 100000000, 0.0004)
    val ord3 = buy(pair, 100000001, 0.00045)

    val ob = OrderBook.empty

    ob.add(ord1, ntpNow)
    ob.add(ord2, ntpNow)
    ob.add(ord3, ntpNow)

    ob.allOrders shouldEqual Seq(SellLimitOrder(ord1.amount, ord1.matcherFee, ord1))
  }

  "partially execute order with zero fee remaining part" in {
    val ord1 = sell(pair, 1500 * Constants.UnitsInWave, 0.0006999)
    val ord2 = sell(pair, 3075248828L, 0.00067634)
    val ord3 = buy(pair, 3075363900L, 0.00073697)

    val ob = OrderBook.empty

    ob.add(ord1, ntpNow)
    ob.add(ord2, ntpNow)
    ob.add(ord3, ntpNow)

    val corrected1 = Order.correctAmount(ord2.amount, ord2.price)
    val leftovers1 = ord3.amount - corrected1
    val corrected2 = Order.correctAmount(leftovers1, ord1.price)
    val restAmount = ord1.amount - corrected2
    // See OrderExecuted.submittedRemainingFee
    val restFee = ord1.matcherFee - LimitOrder.partialFee(ord1.matcherFee, ord1.amount, corrected2)
    ob.allOrders.toSeq shouldEqual Seq(SellLimitOrder(restAmount, restFee, ord1))
  }

  "partially execute order with price > 1 and zero fee remaining part " in {
    val pair = AssetPair(Some(ByteStr("BTC".getBytes)), Some(ByteStr("USD".getBytes)))
    val ord1 = sell(pair, (0.1 * Constants.UnitsInWave).toLong, 1850)
    val ord2 = sell(pair, (0.01 * Constants.UnitsInWave).toLong, 1840)
    val ord3 = buy(pair, (0.0100001 * Constants.UnitsInWave).toLong, 2000)

    val ob = OrderBook.empty

    ob.add(ord1, ntpNow)
    ob.add(ord2, ntpNow)
    ob.add(ord3, ntpNow)

    val restAmount = ord1.amount - (ord3.amount - ord2.amount)
    val restFee    = ord1.matcherFee - LimitOrder.partialFee(ord1.matcherFee, ord1.amount, ord3.amount - ord2.amount)
    ob.allOrders.toSeq shouldEqual Seq(SellLimitOrder(restAmount, restFee, ord1))
  }

  "buy small amount of pricey asset" in {
    val p = AssetPair(Some(ByteStr("WAVES".getBytes)), Some(ByteStr("USD".getBytes)))
    val b = rawBuy(p, 700000L, 280)
    val s = rawSell(p, 30000000000L, 280)

    val ob = OrderBook.empty
    ob.add(s, ntpNow)
    ob.add(b, ntpNow)

    val restSAmount = Order.correctAmount(700000L, 280)
    val restAmount  = 30000000000L - restSAmount
    val restFee     = s.matcherFee - LimitOrder.partialFee(s.matcherFee, s.amount, restSAmount)
    ob.allOrders shouldEqual Seq(SellLimitOrder(restAmount, restFee, s))
  }

  "cleanup expired buy orders" in {
    pending
  }

  "cleanup expired sell orders" in {
    pending
  }

  "aggregate levels for snapshot, preserving order" in {
    pending
  }

  "LimitOrder serialization" in forAll(limitOrderGenerator) { x =>
    val dest = new mutable.ArrayBuilder.ofByte
    SideSnapshot.serialize(dest, x)
    val bb = ByteBuffer.wrap(dest.result())
    SideSnapshot.loFromBytes(bb) shouldBe x
  }

  "SideSnapshot serialization" in forAll(sideSnapshotSerGen) { x =>
    val dest = new mutable.ArrayBuilder.ofByte
    SideSnapshot.serialize(dest, x)
    val bb = ByteBuffer.wrap(dest.result())
    SideSnapshot.fromBytes(bb) shouldBe x
  }

  "LastTrade serialization" in forAll(lastTradeGen) { x =>
    val dest = new mutable.ArrayBuilder.ofByte
    LastTrade.serialize(dest, x)
    val bb = ByteBuffer.wrap(dest.result())
    LastTrade.fromBytes(bb) shouldBe x
  }

  "Snapshot serialization" in forAll(snapshotGen) { x =>
    val dest = new mutable.ArrayBuilder.ofByte
    Snapshot.serialize(dest, x)
    val bb       = ByteBuffer.wrap(dest.result())
    val restored = Snapshot.fromBytes(bb)
    restored.asks shouldBe x.asks
    restored.bids shouldBe x.bids
    restored.lastTrade shouldBe x.lastTrade
  }

  private val sellLevelGen: Gen[Vector[SellLimitOrder]] =
    Gen.containerOf[Vector, SellLimitOrder](sellLimitOrderGenerator)

  private val asksGen: Gen[SideSnapshot] = for {
    n      <- Gen.choose(0, 10)
    levels <- Gen.containerOfN[Vector, Vector[SellLimitOrder]](n, sellLevelGen)
    prices <- Gen.containerOfN[Vector, Long](n, Gen.choose(1, 1000L))
  } yield prices.zip(levels).toMap

  private val buyLevelGen: Gen[Vector[BuyLimitOrder]] =
    Gen.containerOf[Vector, BuyLimitOrder](buyLimitOrderGenerator)

  private val bidsGen: Gen[SideSnapshot] = for {
    n      <- Gen.choose(0, 10)
    levels <- Gen.containerOfN[Vector, Vector[BuyLimitOrder]](n, buyLevelGen)
    prices <- Gen.containerOfN[Vector, Long](n, Gen.choose(1, 1000L))
  } yield prices.zip(levels).toMap

  private val lastTradeGen: Gen[LastTrade] = for {
    price     <- Gen.choose(1, Long.MaxValue)
    amount    <- Gen.choose(1, Long.MaxValue)
    orderType <- orderTypeGenerator
  } yield LastTrade(price, amount, orderType)

  private val snapshotGen: Gen[Snapshot] = for {
    asks      <- asksGen
    bids      <- bidsGen
    lastTrade <- Gen.option(lastTradeGen)
  } yield Snapshot(bids, asks, lastTrade)

  private val sideSnapshotSerGen: Gen[SideSnapshot] = Gen.oneOf(asksGen, bidsGen)
}

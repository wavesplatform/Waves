package com.wavesplatform.matcher.model

import com.wavesplatform.NoShrink
import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.matcher.market.MatcherActor.OrderBookCreated
import com.wavesplatform.matcher.market.OrderBookActor.Snapshot
import com.wavesplatform.matcher.model.MatcherModel.{Level, Price}
import com.wavesplatform.matcher.model.MatcherSerializer._
import com.wavesplatform.state2.ByteStr
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import play.api.libs.json.Json
import scorex.transaction.assets.exchange.AssetPair

import scala.collection.immutable.TreeMap

class EventJsonSpecification extends PropSpec
  with PropertyChecks
  with Matchers
  with MatcherTestData
  with NoShrink {

  val pair = AssetPair(Some(ByteStr("BTC".getBytes)), Some(ByteStr("WAVES".getBytes)))

  val buyLevelGen: Gen[Vector[BuyLimitOrder]] =
    Gen.containerOf[Vector, BuyLimitOrder](buyLimitOrderGenerator)

  val sellLevelGen: Gen[Vector[SellLimitOrder]] =
    Gen.containerOf[Vector, SellLimitOrder](sellLimitOrderGenerator)

  property("Write/Read Bids TreeMap") {
    forAll(buyLevelGen) {xs: Vector[BuyLimitOrder] =>
      val q = TreeMap.empty[Price, Level[BuyLimitOrder]](OrderBook.bidsOrdering) ++ xs.groupBy(_.price)
      val j = Json.toJson(q)
      val res = j.validate[TreeMap[Price, Level[BuyLimitOrder]]]
      res.get should === (q)
      res.get.ordering shouldBe q.ordering
    }
  }

  property("Write/Read Asks TreeMap") {
    forAll(sellLevelGen) {xs: Vector[SellLimitOrder] =>
      val q = TreeMap.empty[Price, Level[SellLimitOrder]](OrderBook.asksOrdering) ++ xs.groupBy(_.price)
      val j = Json.toJson(q)
      val res = j.validate[TreeMap[Price, Level[SellLimitOrder]]]
      res.get should === (q)
      res.get.ordering shouldBe q.ordering
    }
  }


  property("Write/Read OrderBook and Snapshot") {
    forAll(buyLevelGen, sellLevelGen) {(bs: Vector[BuyLimitOrder], ss: Vector[SellLimitOrder]) =>
      val bids = TreeMap.empty[Price, Level[BuyLimitOrder]] ++ bs.groupBy(_.price)
      val asks = TreeMap.empty[Price, Level[SellLimitOrder]] ++ ss.groupBy(_.price)
      val ob = OrderBook(bids, asks)

      val j = Json.toJson(ob)
      val res = j.validate[OrderBook]
      res.get should === (ob)

      val s = Snapshot(ob)

      val js = Json.toJson(s)
      val restored = js.validate[Snapshot]
      restored.get should === (s)
    }
  }

  property("OrderBookCreated json serialization roundtrip") {
    forAll(assetPairGen) { pair: AssetPair =>
      val obc = OrderBookCreated(pair)
      val js = orderBookCreatedFormat.writes(obc)
      val r = js.as[OrderBookCreated]
      obc shouldBe r
    }
  }

}

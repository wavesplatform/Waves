package com.wavesplatform.matcher.model

import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.matcher.market.OrderBookActor.Snapshot
import com.wavesplatform.matcher.model.EventsJson._
import com.wavesplatform.matcher.model.MatcherModel.{Level, Price}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{DoNotDiscover, Matchers, PropSpec}
import play.api.libs.json.Json
import scorex.transaction.assets.exchange.AssetPair

import scala.collection.immutable.TreeMap

@DoNotDiscover
class EventJsonSpecification extends PropSpec
  with PropertyChecks
  with Matchers
  with MatcherTestData {

  val pair = AssetPair(Some("BTC".getBytes), Some("WAVES".getBytes))

  val buyLevelGen: Gen[Vector[BuyLimitOrder]] =
    Gen.containerOf[Vector, BuyLimitOrder](buyLimitOrderGenerator)

  val sellLevelGen: Gen[Vector[SellLimitOrder]] =
    Gen.containerOf[Vector, SellLimitOrder](sellLimitOrderGenerator)

  property("Write/Read TreeMap") {
    forAll(buyLevelGen) {xs: Vector[BuyLimitOrder] =>
      val q = TreeMap.empty[Price, Level[BuyLimitOrder]] ++ xs.groupBy(_.price)
      val j = Json.toJson(q)
      val res = j.validate[TreeMap[Price, Level[BuyLimitOrder]]]
      res.get should === (q)
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

      val cache = Map[String, (Long, Long)]("account1" -> (100L, 0L), "account2" -> (105L, 30L))
      val s = Snapshot(ob, cache)

      val js = Json.toJson(s)
      val restored = js.validate[Snapshot]
      restored.get should === (s)
    }
  }

}

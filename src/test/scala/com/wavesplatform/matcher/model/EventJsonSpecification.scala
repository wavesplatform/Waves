package com.wavesplatform.matcher.model

import com.wavesplatform.NoShrink
import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.matcher.market.MatcherActor.OrderBookCreated
import com.wavesplatform.matcher.market.OrderBookActor.Snapshot
import com.wavesplatform.matcher.model.EventSerializers._
import com.wavesplatform.matcher.model.MatcherModel.{Level, Price}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import play.api.libs.json.Json
import com.wavesplatform.transaction.assets.exchange.AssetPair

import scala.collection.immutable.TreeMap

class EventJsonSpecification extends PropSpec with PropertyChecks with Matchers with MatcherTestData with NoShrink {

  val buyLevelGen: Gen[Vector[BuyLimitOrder]] =
    Gen.containerOf[Vector, BuyLimitOrder](buyLimitOrderGenerator)

  val sellLevelGen: Gen[Vector[SellLimitOrder]] =
    Gen.containerOf[Vector, SellLimitOrder](sellLimitOrderGenerator)

  property("Write/Read Bids TreeMap") {
    forAll(buyLevelGen) { xs: Vector[BuyLimitOrder] =>
      val q   = TreeMap.empty[Price, Level[BuyLimitOrder]](OrderBook.bidsOrdering) ++ xs.groupBy(_.price)
      val j   = Json.toJson(q)
      val res = j.validate[TreeMap[Price, Level[BuyLimitOrder]]]
      res.get should ===(q)
      res.get.ordering shouldBe q.ordering
    }
  }

  property("Write/Read Asks TreeMap") {
    forAll(sellLevelGen) { xs: Vector[SellLimitOrder] =>
      val q   = TreeMap.empty[Price, Level[SellLimitOrder]](OrderBook.asksOrdering) ++ xs.groupBy(_.price)
      val j   = Json.toJson(q)
      val res = j.validate[TreeMap[Price, Level[SellLimitOrder]]]
      res.get should ===(q)
      res.get.ordering shouldBe q.ordering
    }
  }

  property("Write/Read OrderBook and Snapshot") {
    forAll(buyLevelGen, sellLevelGen) { (bs: Vector[BuyLimitOrder], ss: Vector[SellLimitOrder]) =>
      val bids = TreeMap.empty[Price, Level[BuyLimitOrder]] ++ bs.groupBy(_.price)
      val asks = TreeMap.empty[Price, Level[SellLimitOrder]] ++ ss.groupBy(_.price)
      val ob   = OrderBook(bids, asks)

      val j   = Json.toJson(ob)
      val res = j.validate[OrderBook]
      res.get should ===(ob)

      val s = Snapshot(2, ob)

      val js       = Json.toJson(s)
      val restored = js.validate[Snapshot]
      restored.get should ===(s)
    }
  }

  property("OrderBookCreated json serialization roundtrip") {
    forAll(assetPairGen) { pair: AssetPair =>
      val obc = OrderBookCreated(pair)
      val js  = orderBookCreatedFormat.writes(obc)
      val r   = js.as[OrderBookCreated]
      obc shouldBe r
    }
  }

  property("Backward compatibility Reads for OrderCancelled") {
    val rawJson =
      """{
        |  "o": {
        |    "amount": 1,
        |    "price": 112300000,
        |    "order": {
        |       "id" : "12C6pHxbbiYXaRtpmDnVoyiwb78DFztqwPmorogYzdT5",
        |       "sender" : "3Hi5pLwXXo3WeGEg2HgeDcy4MjQRTgz7WRx",
        |       "senderPublicKey" : "Gk2vtWGKPRqSQTqc72ZJhQ1y2T25AioAGNvKSUXJpXWD",
        |       "matcherPublicKey" : "FkV3y43B4SAXkSL31SXFZU5xm5bonRtRVNU1sQzwpVhm",
        |       "assetPair" : {
        |         "amountAsset" : "ASZc9p4DLkoPb1qKAkLzdZJB7S7hkBf95mwnYdBW5pCc",
        |         "priceAsset" : null
        |       },
        |       "orderType" : "buy",
        |       "price" : 112300000,
        |       "amount" : 8,
        |       "timestamp" : 1530800388321,
        |       "expiration" : 1533392387321,
        |       "matcherFee" : 300000,
        |       "signature" : "uckvU4TsyP9DQUjZABcnLm3i2w6LvaJBTcEw23mZDC8u18roGQazxV69qMd82kvm2SjZsxoZQqUMBHcJLwYBbWq"
        |     }
        |  }
        |}""".stripMargin

    val actual = EventSerializers.orderCancelledFormat.reads(Json.parse(rawJson))
    actual.isSuccess shouldBe true
    actual.get.unmatchable shouldBe false
  }

  property("Reads for OrderCancelled with unmatchable") {
    val rawJson =
      """{
        |  "o": {
        |    "amount": 1,
        |    "price": 112300000,
        |    "order": {
        |       "id" : "12C6pHxbbiYXaRtpmDnVoyiwb78DFztqwPmorogYzdT5",
        |       "sender" : "3Hi5pLwXXo3WeGEg2HgeDcy4MjQRTgz7WRx",
        |       "senderPublicKey" : "Gk2vtWGKPRqSQTqc72ZJhQ1y2T25AioAGNvKSUXJpXWD",
        |       "matcherPublicKey" : "FkV3y43B4SAXkSL31SXFZU5xm5bonRtRVNU1sQzwpVhm",
        |       "assetPair" : {
        |         "amountAsset" : "ASZc9p4DLkoPb1qKAkLzdZJB7S7hkBf95mwnYdBW5pCc",
        |         "priceAsset" : null
        |       },
        |       "orderType" : "buy",
        |       "price" : 112300000,
        |       "amount" : 8,
        |       "timestamp" : 1530800388321,
        |       "expiration" : 1533392387321,
        |       "matcherFee" : 300000,
        |       "signature" : "uckvU4TsyP9DQUjZABcnLm3i2w6LvaJBTcEw23mZDC8u18roGQazxV69qMd82kvm2SjZsxoZQqUMBHcJLwYBbWq"
        |     }
        |  },
        |  "unmatchable": true
        |}""".stripMargin

    val actual = EventSerializers.orderCancelledFormat.reads(Json.parse(rawJson)).get
    actual.unmatchable shouldBe true
  }

}

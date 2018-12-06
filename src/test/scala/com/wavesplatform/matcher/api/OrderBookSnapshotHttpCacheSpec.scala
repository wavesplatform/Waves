package com.wavesplatform.matcher.api

import java.nio.charset.StandardCharsets

import akka.http.scaladsl.model.{HttpEntity, HttpResponse}
import com.wavesplatform.OrderOps._
import com.wavesplatform.matcher.model.MatcherModel.Price
import com.wavesplatform.matcher.model._
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.{NTPTime, TransactionGenBase}
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, Matchers}

import scala.collection.immutable.TreeMap
import scala.concurrent.duration._

class OrderBookSnapshotHttpCacheSpec extends FreeSpec with Matchers with TransactionGenBase with NTPTime {

  private val defaultAssetPair = AssetPair(None, Some(ByteStr("asset".getBytes)))

  "OrderBookSnapshotHttpCache" - {
    "should cache" in using(createDefaultCache) { cache =>
      def get = cache.get(defaultAssetPair, Some(1))

      val a = get
      val b = get

      a shouldBe b
    }

    "should not drop the cache if the timeout after an access was not reached" in using(createDefaultCache) { cache =>
      def get = cache.get(defaultAssetPair, Some(1))

      val a = get
      Thread.sleep(30)
      val b = get

      a shouldBe b
    }

    "should drop the cache after timeout" in using(createDefaultCache) { cache =>
      def get = cache.get(defaultAssetPair, Some(1))

      val a = get
      Thread.sleep(70)
      val b = get

      a shouldNot be(b)
    }

    "should aggregate levels and preserve right order" - {
      // Two levels: one is aggregated and one is not

      val askLimitOrders = Gen
        .containerOfN[Vector, Order](2, orderGen)
        .map { xs =>
          val r = xs.head +: xs.tail.map(_.updatePrice(price = xs.head.price - 1))
          r.map(x => SellLimitOrder(x.amount, x.matcherFee, x)).groupBy(_.price)
        }
        .sample
        .get

      val bidLimitOrders = Gen
        .containerOfN[Vector, Order](2, orderGen)
        .map { xs =>
          val r = xs.head +: xs.tail.map(_.updatePrice(xs.head.price + 1))
          r.map(x => BuyLimitOrder(x.amount, x.matcherFee, x)).groupBy(_.price)
        }
        .sample
        .get

      "asks" in using {
        new OrderBookSnapshotHttpCache(
          OrderBookSnapshotHttpCache.Settings(1.minute, List(3, 9)),
          ntpTime, { _ =>
            Some(
              OrderBook(
                bids = TreeMap.empty,
                asks = TreeMap(askLimitOrders.toSeq: _*)(OrderBook.asksOrdering)
              )
            )
          }
        )
      } { cache =>
        val ob = orderBookFrom(cache.get(defaultAssetPair, Some(3)))

        withClue("bids size")(ob.bids shouldBe empty)
        withClue("asks size")(ob.asks.size shouldBe 2)

        def checkHasAggregatedAmount(x: LevelAgg): Unit = withClue(s"${x.price} should have the aggregated amount") {
          x shouldBe LevelAgg(askLimitOrders(x.price).map(_.amount).sum, x.price)
        }

        ob.asks.foreach(checkHasAggregatedAmount)

        withClue(s"asks have ascending order (${ob.asks.map(_.price).mkString(", ")})") {
          ob.asks.zip(ob.asks.tail).foreach {
            case (prev, next) => prev.price shouldBe <(next.price)
          }
        }
      }

      "bids" in using {
        new OrderBookSnapshotHttpCache(
          OrderBookSnapshotHttpCache.Settings(1.minute, List(3, 9)),
          ntpTime, { _ =>
            Some(
              OrderBook(
                bids = TreeMap(bidLimitOrders.toSeq: _*)(OrderBook.bidsOrdering),
                asks = TreeMap.empty
              )
            )
          }
        )
      } { cache =>
        val ob = orderBookFrom(cache.get(defaultAssetPair, Some(3)))

        withClue("bids size")(ob.bids.size shouldBe 2)
        withClue("asks size")(ob.asks shouldBe empty)

        def checkHasAggregatedAmount(x: LevelAgg): Unit = withClue(s"${x.price} should have the aggregated amount") {
          x shouldBe LevelAgg(bidLimitOrders(x.price).map(_.amount).sum, x.price)
        }

        ob.bids.foreach(checkHasAggregatedAmount)

        withClue(s"bids have descending order (${ob.bids.map(_.price).mkString(", ")})") {
          ob.bids.zip(ob.bids.tail).foreach {
            case (prev, next) => prev.price shouldBe >(next.price)
          }
        }
      }
    }

    "should return the nearest depth cache" - {
      // Two levels: one is aggregated and one is not
      val bidLimitOrders = randomBidLimitOrders
      using {
        new OrderBookSnapshotHttpCache(
          OrderBookSnapshotHttpCache.Settings(1.minute, List(3, 9)),
          ntpTime, { _ =>
            Some(
              OrderBook(
                bids = TreeMap(bidLimitOrders.toSeq: _*),
                asks = TreeMap.empty
              )
            )
          }
        )
      } { cache =>
        "None -> 9" in {
          val ob = orderBookFrom(cache.get(defaultAssetPair, None))
          ob.bids.size shouldBe 9
        }

        Seq(
          0  -> 3,
          1  -> 3,
          3  -> 3,
          5  -> 9,
          10 -> 9
        ).foreach {
          case (depth, expectedSize) =>
            s"$depth -> $expectedSize" in {
              val ob = orderBookFrom(cache.get(defaultAssetPair, Some(depth)))
              ob.bids.size shouldBe expectedSize
            }
        }
      }
    }

    "should clear all depth caches after invalidate" in {
      val bidLimitOrders = randomBidLimitOrders
      val depths         = List(1, 3, 7, 9)
      using {
        new OrderBookSnapshotHttpCache(
          OrderBookSnapshotHttpCache.Settings(1.minute, depths),
          ntpTime, { _ =>
            Some(
              OrderBook(
                bids = TreeMap(bidLimitOrders.toSeq: _*),
                asks = TreeMap.empty
              )
            )
          }
        )
      } { cache =>
        val prev = depths.map(x => x -> orderBookFrom(cache.get(defaultAssetPair, Some(x)))).toMap

        Thread.sleep(100)
        cache.invalidate(defaultAssetPair)

        depths.foreach { depth =>
          withClue(s"cache for depth=$depth was invalidated") {
            val curr = orderBookFrom(cache.get(defaultAssetPair, Some(depth)))
            curr.timestamp shouldNot be(prev(depth).timestamp)
          }
        }
      }
    }
  }

  private def createDefaultCache = new OrderBookSnapshotHttpCache(OrderBookSnapshotHttpCache.Settings(50.millis, List(3, 9)), ntpTime, _ => None)

  private def orderBookFrom(x: HttpResponse): OrderBookResult = JsonSerializer.deserialize[OrderBookResult](
    x.entity
      .asInstanceOf[HttpEntity.Strict]
      .getData()
      .decodeString(StandardCharsets.UTF_8)
  )

  private def randomBidLimitOrders: Map[Price, Vector[BuyLimitOrder]] =
    Gen
      .containerOfN[Vector, Order](10, orderGen)
      .map { xs =>
        xs.map(x => BuyLimitOrder(x.amount, x.matcherFee, x)).groupBy(_.price)
      }
      .sample
      .get

  private def using[T <: AutoCloseable](create: => T)(f: T => Unit): Unit = {
    val x = create
    try {
      f(x)
    } finally {
      x.close()
    }
  }
}

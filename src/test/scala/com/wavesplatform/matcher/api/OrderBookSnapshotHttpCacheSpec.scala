package com.wavesplatform.matcher.api

import java.nio.charset.StandardCharsets

import akka.http.scaladsl.model.{HttpEntity, HttpResponse}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.matcher.model._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.{NTPTime, TransactionGenBase}
import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent.duration._

class OrderBookSnapshotHttpCacheSpec extends FreeSpec with Matchers with TransactionGenBase with NTPTime {

  private val defaultAssetPair = AssetPair(Waves, IssuedAsset(ByteStr("asset".getBytes)))

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

    "should return the nearest depth cache" - {
      // Two levels: one is aggregated and one is not
      using {
        new OrderBookSnapshotHttpCache(
          OrderBookSnapshotHttpCache.Settings(1.minute, List(3, 9)),
          ntpTime,
          _ =>
            Some(
              OrderBook.AggregatedSnapshot(
                Seq.tabulate(15)(i => LevelAgg(200 - i * 10, 1000 - 10 * i)),
                Seq.tabulate(15)(i => LevelAgg(200 - i * 10, 1000 - 10 * i)),
              ))
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
      val depths = List(1, 3, 7, 9)
      using {
        new OrderBookSnapshotHttpCache(OrderBookSnapshotHttpCache.Settings(1.minute, depths), ntpTime, _ => None)
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

  private def using[T <: AutoCloseable](create: => T)(f: T => Unit): Unit = {
    val x = create
    try {
      f(x)
    } finally {
      x.close()
    }
  }
}

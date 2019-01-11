package com.wavesplatform.matcher.api

import java.util.concurrent.ScheduledFuture

import akka.http.scaladsl.model.HttpResponse
import com.google.common.cache.{CacheBuilder, CacheLoader}
import com.wavesplatform.matcher.api.OrderBookSnapshotHttpCache.Settings
import com.wavesplatform.matcher.market.OrderBookActor.GetOrderBookResponse
import com.wavesplatform.matcher.model.MatcherModel.{Level, Price}
import com.wavesplatform.matcher.model.{LevelAgg, LimitOrder, OrderBook}
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.utils.Time
import kamon.Kamon

import scala.concurrent.duration._

class OrderBookSnapshotHttpCache(settings: Settings, time: Time, orderBookSnapshot: AssetPair => Option[OrderBook]) extends AutoCloseable {
  import OrderBookSnapshotHttpCache._

  private val depthRanges = settings.depthRanges.sorted
  private val maxDepth    = depthRanges.max

  private val orderBookSnapshotCache = CacheBuilder
    .newBuilder()
    .expireAfterAccess(settings.cacheTimeout.length, settings.cacheTimeout.unit)
    .build[Key, HttpResponse](new CacheLoader[Key, HttpResponse] {
      override def load(key: Key): HttpResponse = {
        def aggregateLevel(l: (Price, Level[LimitOrder])) = LevelAgg(l._2.foldLeft(0L)((b, o) => b + o.amount), l._1)

        val orderBook = orderBookSnapshot(key.pair).getOrElse(OrderBook.empty)
        GetOrderBookResponse(
          time.correctedTime(),
          key.pair,
          orderBook.bids.view.take(key.depth).map(aggregateLevel).toSeq,
          orderBook.asks.view.take(key.depth).map(aggregateLevel).toSeq
        ).toHttpResponse
      }
    })

  private val statsScheduler: ScheduledFuture[_] = {
    val period       = 3.seconds
    val requestStats = Kamon.histogram("matcher.http.ob.cache.req")
    val hitStats     = Kamon.histogram("matcher.http.ob.cache.hit")
    Kamon
      .scheduler()
      .scheduleWithFixedDelay(
        { () =>
          val stats = orderBookSnapshotCache.stats()
          requestStats.record(stats.requestCount())
          hitStats.record((stats.hitRate() * 100).toLong)
        },
        period.toSeconds,
        period.toSeconds,
        period.unit
      )
  }

  def get(pair: AssetPair, depth: Option[Int]): HttpResponse = {
    val nearestDepth = depth
      .flatMap(desiredDepth => depthRanges.find(_ >= desiredDepth))
      .getOrElse(maxDepth)

    orderBookSnapshotCache.get(Key(pair, nearestDepth))
  }

  def invalidate(pair: AssetPair): Unit = {
    import scala.collection.JavaConverters._
    orderBookSnapshotCache.invalidateAll(depthRanges.map(Key(pair, _)).asJava)
  }

  override def close(): Unit = {
    statsScheduler.cancel(true)
  }
}

object OrderBookSnapshotHttpCache {
  case class Settings(cacheTimeout: FiniteDuration, depthRanges: List[Int])

  private case class Key(pair: AssetPair, depth: Int)
}

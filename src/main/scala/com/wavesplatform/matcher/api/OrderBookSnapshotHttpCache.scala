package com.wavesplatform.matcher.api

import akka.http.scaladsl.model.HttpResponse
import com.google.common.cache.{CacheBuilder, CacheLoader}
import com.wavesplatform.matcher.api.OrderBookSnapshotHttpCache.Settings
import com.wavesplatform.matcher.market.OrderBookActor.GetOrderBookResponse
import com.wavesplatform.matcher.model.MatcherModel.{Level, Price}
import com.wavesplatform.matcher.model.{LevelAgg, LimitOrder, OrderBook}
import scorex.transaction.assets.exchange.AssetPair
import scorex.utils.NTP

import scala.concurrent.duration.FiniteDuration

class OrderBookSnapshotHttpCache(settings: Settings, orderBookSnapshot: AssetPair => Option[OrderBook]) {
  import OrderBookSnapshotHttpCache._

  private val depthRanges = settings.depthRanges.sorted
  private val maxDepth    = depthRanges.max

  private val orderBookSnapshotCache = CacheBuilder
    .newBuilder()
    .expireAfterAccess(settings.cacheTimeout.length, settings.cacheTimeout.unit)
    .build[Key, HttpResponse](new CacheLoader[Key, HttpResponse] {
      override def load(key: Key): HttpResponse = {
        def aggregateLevel(l: (Price, Level[LimitOrder])) = LevelAgg(l._1, l._2.foldLeft(0L)((b, o) => b + o.amount))

        val orderBook = orderBookSnapshot(key.pair).getOrElse(OrderBook.empty)
        GetOrderBookResponse(
          NTP.correctedTime(),
          key.pair,
          orderBook.bids.view.take(key.depth).map(aggregateLevel).toSeq,
          orderBook.asks.view.take(key.depth).map(aggregateLevel).toSeq
        ).toHttpResponse
      }
    })

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
}

object OrderBookSnapshotHttpCache {
  case class Settings(cacheTimeout: FiniteDuration, depthRanges: List[Int])

  private case class Key(pair: AssetPair, depth: Int)
}

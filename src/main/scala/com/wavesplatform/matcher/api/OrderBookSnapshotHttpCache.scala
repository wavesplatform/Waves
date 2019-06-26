package com.wavesplatform.matcher.api

import java.util.concurrent.ScheduledFuture

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse}
import cats.implicits._
import com.google.common.cache.{CacheBuilder, CacheLoader}
import com.wavesplatform.matcher.api.OrderBookSnapshotHttpCache.Settings
import com.wavesplatform.matcher.model.MatcherModel.{DecimalsFormat, Denormalized, Normalized}
import com.wavesplatform.matcher.model.{OrderBook, OrderBookResult}
import com.wavesplatform.transaction.AssetId
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.utils.Time
import kamon.Kamon

import scala.collection.JavaConverters._
import scala.concurrent.duration._

class OrderBookSnapshotHttpCache(settings: Settings,
                                 time: Time,
                                 assetDecimals: Option[AssetId] => Int,
                                 orderBookSnapshot: AssetPair => Option[OrderBook.AggregatedSnapshot])
    extends AutoCloseable {
  import OrderBookSnapshotHttpCache._

  private val depthRanges = settings.depthRanges.sorted
  private val maxDepth    = depthRanges.max

  private val orderBookSnapshotCache = CacheBuilder
    .newBuilder()
    .expireAfterAccess(settings.cacheTimeout.length, settings.cacheTimeout.unit)
    .build[Key, HttpResponse](
      new CacheLoader[Key, HttpResponse] {
        override def load(key: Key): HttpResponse = {

          val orderBook = orderBookSnapshot(key.pair).getOrElse(OrderBook.AggregatedSnapshot())

          val assetPairDecimals = key.format match {
            case Denormalized => Some(assetDecimals(key.pair.amountAsset) -> assetDecimals(key.pair.priceAsset))
            case _            => None
          }

          val entity =
            OrderBookResult(
              time.correctedTime(),
              key.pair,
              orderBook.bids.take(key.depth),
              orderBook.asks.take(key.depth),
              assetPairDecimals
            )

          HttpResponse(
            entity = HttpEntity(
              ContentTypes.`application/json`,
              OrderBookResult.toJson(entity)
            )
          )
        }
      }
    )

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

  def get(pair: AssetPair, depth: Option[Int], format: DecimalsFormat = Normalized): HttpResponse = {
    val nearestDepth = depth
      .flatMap(desiredDepth => depthRanges.find(_ >= desiredDepth))
      .getOrElse(maxDepth)

    orderBookSnapshotCache.get(Key(pair, nearestDepth, format))
  }

  def invalidate(pair: AssetPair): Unit = {
    orderBookSnapshotCache.invalidateAll {
      depthRanges
        .flatMap(depth => List(Normalized, Denormalized).map(depth -> _))
        .map { case (depth, format) => Key(pair, depth, format) }
        .asJava
    }
  }

  override def close(): Unit = {
    statsScheduler.cancel(true)
  }
}

object OrderBookSnapshotHttpCache {
  case class Settings(cacheTimeout: FiniteDuration, depthRanges: List[Int])
  private case class Key(pair: AssetPair, depth: Int, format: DecimalsFormat)
}

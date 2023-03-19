package com.wavesplatform.ride.runner.stats

import com.github.benmanes.caffeine.cache.RemovalCause
import com.github.benmanes.caffeine.cache.stats.{CacheStats, StatsCounter}
import kamon.Kamon

// Tags that is easier to show in Grafana than with original KamonStatsCounter.
class KamonCaffeineStats(name: String) extends StatsCounter {
  val hitsCounter     = Kamon.counter("cache.hits").withTag("name", name)
  val missesCounter   = Kamon.counter("cache.misses").withTag("name", name)
  val evictionCount   = Kamon.counter("cache.evictions").withTag("name", name)
  val loadSuccessTime = Kamon.timer("cache.load-time.success").withTag("name", name)
  val loadFailureTime = Kamon.timer("cache.load-time.failure").withTag("name", name)
  val evictionWeight  = Kamon.counter("cache.eviction.weight").withTag("name", name)
  val evictionWeightInstruments = RemovalCause
    .values()
    .map(cause => cause -> evictionWeight.withTag("eviction.cause", cause.name()))
    .toMap

  override def recordHits(count: Int): Unit = hitsCounter.increment(count)

  override def recordMisses(count: Int): Unit = missesCounter.increment(count)

  override def recordLoadSuccess(loadTime: Long): Unit = loadSuccessTime.record(loadTime)

  override def recordLoadFailure(loadTime: Long): Unit = loadFailureTime.record(loadTime)

  override def recordEviction(weight: Int, cause: RemovalCause): Unit = {
    evictionCount.increment()
    evictionWeightInstruments.get(cause).map(_.increment(weight))
  }

  /** Overrides the snapshot method and returns stubbed CacheStats. When using KamonStatsCounter, it is assumed that you are using a reporter, and are
    * not going to be printing or logging the stats.
    */
  override def snapshot() = CacheStats.of(0, 0, 0, 0, 0, 0, 0)
}

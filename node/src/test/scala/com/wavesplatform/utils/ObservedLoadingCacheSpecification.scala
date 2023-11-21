package com.wavesplatform.utils

import com.google.common.base.Ticker
import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.utils.ObservedLoadingCacheSpecification.FakeTicker
import monix.reactive.Observable
import monix.reactive.subjects.PublishToOneSubject

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.*

class ObservedLoadingCacheSpecification extends FreeSpec {
  private val ExpiringTime = 10.minutes

  import monix.execution.Scheduler.Implicits.global

  "notifies" - {
    "on refresh" in test { (loadingCache, changes, _) =>
      collectChangedKeys(changes) {
        loadingCache.refresh("foo")
      } shouldEqual Seq("foo")
    }

    "on put" in test { (loadingCache, changes, _) =>
      collectChangedKeys(changes) {
        loadingCache.put("foo", 10)
      } shouldEqual Seq("foo")
    }

    "on putAll" in test { (loadingCache, changes, _) =>
      collectChangedKeys(changes) {
        loadingCache.putAll(Map[String, Integer]("foo" -> 10, "bar" -> 11).asJava)
      } shouldEqual Seq("foo", "bar")
    }

    "on invalidate" in test { (loadingCache, changes, _) =>
      collectChangedKeys(changes) {
        loadingCache.invalidate("foo")
      } shouldEqual Seq("foo")
    }

    "on invalidateAll" in test { (loadingCache, changes, _) =>
      collectChangedKeys(changes) {
        loadingCache.invalidateAll(Seq("foo", "bar").asJava)
      } shouldEqual Seq("foo", "bar")
    }
  }

  "don't notify" - {
    "on cache expiration" in test { (loadingCache, changes, ticker) =>
      collectChangedKeys(changes) {
        loadingCache.put("foo", 1)
        ticker.advance(ExpiringTime.toMillis + 100, TimeUnit.MILLISECONDS)
      } shouldEqual Seq("foo")
    }
  }

  private def test(f: (LoadingCache[String, Integer], Observable[String], FakeTicker) => Unit): Unit = {
    val changes = PublishToOneSubject[String]
    val ticker  = new FakeTicker()

    val delegate = CacheBuilder
      .newBuilder()
      .expireAfterWrite(ExpiringTime.toMillis, TimeUnit.MILLISECONDS)
      .ticker(ticker)
      .build[String, Integer](new CacheLoader[String, Integer] {
        override def load(key: String): Integer = key.length
      })

    val loadingCache = new ObservedLoadingCache(delegate, changes)

    f(loadingCache, changes, ticker)
  }

  private def collectChangedKeys(changes: Observable[String])(action: => Unit): Seq[String] = {
    val events = changes.toListL.runToFuture
    action
    Await.result(events, 5.seconds)
  }
}

private object ObservedLoadingCacheSpecification {

  // see https://github.com/google/guava/blob/master/guava-testlib/src/com/google/common/testing/FakeTicker.java
  class FakeTicker extends Ticker {
    private val nanos                  = new AtomicLong()
    private var autoIncrementStepNanos = 0L

    def advance(time: Long, timeUnit: TimeUnit): FakeTicker = advance(timeUnit.toNanos(time))
    def advance(nanoseconds: Long): FakeTicker = {
      nanos.addAndGet(nanoseconds)
      this
    }

    def setAutoIncrementStep(autoIncrementStep: Long, timeUnit: TimeUnit): FakeTicker = {
      require(autoIncrementStep >= 0, "May not auto-increment by a negative amount")
      this.autoIncrementStepNanos = timeUnit.toNanos(autoIncrementStep)
      this
    }

    override def read: Long = nanos.getAndAdd(autoIncrementStepNanos)
  }
}

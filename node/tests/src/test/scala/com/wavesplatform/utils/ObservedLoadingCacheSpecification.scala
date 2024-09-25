package com.wavesplatform.utils

import com.google.common.base.Ticker
import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicLong
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.utils.ObservedLoadingCacheSpecification.FakeTicker
import monix.execution.Ack
import monix.reactive.Observer
import org.scalamock.scalatest.MockFactory

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.*

class ObservedLoadingCacheSpecification extends FreeSpec with MockFactory {
  private val ExpiringTime = 10.minutes

  "notifies" - {
    "on refresh" in test { (loadingCache, changes, _) =>
      (changes.onNext _).expects("foo").returning(Future.successful(Ack.Continue)).once()

      loadingCache.refresh("foo")
    }

    "on put" in test { (loadingCache, changes, _) =>
      (changes.onNext _).expects("foo").returning(Future.successful(Ack.Continue)).once()

      loadingCache.put("foo", 10)
    }

    "on putAll" in test { (loadingCache, changes, _) =>
      (changes.onNext _).expects("foo").returning(Future.successful(Ack.Continue)).once()
      (changes.onNext _).expects("bar").returning(Future.successful(Ack.Continue)).once()

      loadingCache.putAll(Map[String, Integer]("foo" -> 10, "bar" -> 11).asJava)
    }

    "on invalidate" in test { (loadingCache, changes, _) =>
      (changes.onNext _).expects("foo").returning(Future.successful(Ack.Continue)).once()

      loadingCache.invalidate("foo")
    }

    "on invalidateAll" in test { (loadingCache, changes, _) =>
      (changes.onNext _).expects("foo").returning(Future.successful(Ack.Continue)).once()
      (changes.onNext _).expects("bar").returning(Future.successful(Ack.Continue)).once()

      loadingCache.invalidateAll(Seq("foo", "bar").asJava)
    }
  }

  "don't notify" - {
    "on cache expiration" in test { (loadingCache, changes, ticker) =>
      (changes.onNext _).expects("foo").returning(Future.successful(Ack.Continue)).once()
      loadingCache.put("foo", 1)
      ticker.advance(ExpiringTime.toMillis + 100, TimeUnit.MILLISECONDS)
    }
  }

  private def test(f: (LoadingCache[String, Integer], Observer[String], FakeTicker) => Unit): Unit = {
    val changes = mock[Observer[String]]
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

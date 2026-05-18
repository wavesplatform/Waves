package com.wavesplatform.utils

import com.google.common.base.Ticker
import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import com.wavesplatform.test.FreeSpec
import monix.reactive.Observer
import org.apache.commons.io.output.WriterOutputStream

import java.io.{PrintStream, StringWriter}
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.*
import scala.jdk.DurationConverters.*

class ObservedLoadingCacheSpecification extends FreeSpec {
  import com.wavesplatform.utils.ObservedLoadingCacheSpecification.FakeTicker
  private val ExpiringTime = 10.minutes

  "notifies" - {
    "on refresh" in test { (loadingCache, changes, _) =>
      loadingCache.refresh("foo")
      changes.toString shouldBe "0:  --> foo\n"
    }

    "on put" in test { (loadingCache, changes, _) =>
      loadingCache.put("foo", 10)
      changes.toString shouldBe "0:  --> foo\n"
    }

    "on putAll" in test { (loadingCache, changes, _) =>
      loadingCache.putAll(Map[String, Integer]("foo" -> 10, "bar" -> 11).asJava)
      changes.toString shouldBe "0:  --> foo\n1:  --> bar\n"
    }

    "on invalidate" in test { (loadingCache, changes, _) =>
      loadingCache.invalidate("foo")
      changes.toString shouldBe "0:  --> foo\n"
    }

    "on invalidateAll" in test { (loadingCache, changes, _) =>
      loadingCache.invalidateAll(Seq("foo", "bar").asJava)
      changes.toString shouldBe "0:  --> foo\n1:  --> bar\n"
    }
  }

  "don't notify" - {
    "on cache expiration" in test {
      (loadingCache, changes, ticker) =>
        loadingCache.put("foo", 1)
        ticker.advance(ExpiringTime.toMillis + 100, TimeUnit.MILLISECONDS)
      changes.toString shouldBe "0:  --> foo\n"
    }
  }

  private def test(f: (LoadingCache[String, Integer], StringWriter, FakeTicker) => Unit): Unit = {
    val ticker = new FakeTicker()

    val delegate = CacheBuilder
      .newBuilder()
      .expireAfterWrite(ExpiringTime.toJava)
      .ticker(ticker)
      .build[String, Integer](new CacheLoader[String, Integer] {
        override def load(key: String): Integer = key.length
      })

    val sw = new StringWriter()
    val loadingCache = new ObservedLoadingCache(
      delegate,
      Observer.dump("", new PrintStream(WriterOutputStream.builder().setWriter(sw).setWriteImmediately(true).get()))
    )

    f(loadingCache, sw, ticker)
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

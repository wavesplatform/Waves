package com.wavesplatform.settings

import com.typesafe.config.ConfigFactory
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.MatcherSettings.EventsQueueSettings
import com.wavesplatform.matcher.api.OrderBookSnapshotHttpCache
import com.wavesplatform.matcher.queue.{KafkaMatcherQueue, LocalMatcherQueue}
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class MatcherSettingsSpecification extends FlatSpec with Matchers {
  "MatcherSettings" should "read values" in {
    val config = loadConfig(ConfigFactory.parseString("""waves {
        |  directory = /waves
        |  matcher {
        |    enable = yes
        |    account = 3Mqjki7bLtMEBRCYeQis39myp9B4cnooDEX
        |    bind-address = 127.0.0.1
        |    port = 6886
        |    min-order-fee = 100000
        |    order-match-tx-fee = 100000
        |    snapshots-interval = 999
        |    make-snapshots-at-start = yes
        |    snapshots-loading-timeout = 423s
        |    start-events-processing-timeout = 543s
        |    rest-order-limit = 100
        |    price-assets = [
        |      WAVES
        |      8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS
        |      DHgwrRvVyqJsepd32YbBqUeDH4GJ1N984X8QoekjgH8J
        |    ]
        |    blacklisted-assets = ["a"]
        |    blacklisted-names = ["b"]
        |    blacklisted-addresses = [
        |      3N5CBq8NYBMBU3UVS3rfMgaQEpjZrkWcBAD
        |    ]
        |    order-book-snapshot-http-cache {
        |      cache-timeout = 11m
        |      depth-ranges = [1, 5, 333]
        |    }
        |    balance-watching-buffer-interval = 33s
        |    events-queue {
        |      type = "kafka"
        |
        |      local {
        |        polling-interval = 1d
        |        max-elements-per-poll = 99
        |        clean-before-consume = no
        |      }
        |
        |      kafka {
        |        topic = "some-events"
        |
        |        consumer {
        |          buffer-size = 100
        |          min-backoff = 11s
        |          max-backoff = 2d
        |        }
        |
        |        producer.buffer-size = 200
        |      }
        |    }
        |  }
        |}""".stripMargin))

    val settings = MatcherSettings.fromConfig(config)
    settings.enable should be(true)
    settings.account should be("3Mqjki7bLtMEBRCYeQis39myp9B4cnooDEX")
    settings.bindAddress should be("127.0.0.1")
    settings.port should be(6886)
    settings.minOrderFee should be(100000)
    settings.orderMatchTxFee should be(100000)
    settings.journalDataDir should be("/waves/matcher/journal")
    settings.snapshotsDataDir should be("/waves/matcher/snapshots")
    settings.snapshotsInterval should be(999)
    settings.makeSnapshotsAtStart should be(true)
    settings.snapshotsLoadingTimeout should be(423.seconds)
    settings.startEventsProcessingTimeout should be(543.seconds)
    settings.maxOrdersPerRequest should be(100)
    settings.priceAssets should be(Seq("WAVES", "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS", "DHgwrRvVyqJsepd32YbBqUeDH4GJ1N984X8QoekjgH8J"))
    settings.blacklistedAssets shouldBe Set("a")
    settings.blacklistedNames.map(_.pattern.pattern()) shouldBe Seq("b")
    settings.blacklistedAddresses shouldBe Set("3N5CBq8NYBMBU3UVS3rfMgaQEpjZrkWcBAD")
    settings.orderBookSnapshotHttpCache shouldBe OrderBookSnapshotHttpCache.Settings(
      cacheTimeout = 11.minutes,
      depthRanges = List(1, 5, 333)
    )
    settings.balanceWatchingBufferInterval should be(33.seconds)
    settings.eventsQueue shouldBe EventsQueueSettings(
      tpe = "kafka",
      local = LocalMatcherQueue.Settings(1.day, 99, cleanBeforeConsume = false),
      kafka = KafkaMatcherQueue.Settings(
        "some-events",
        KafkaMatcherQueue.ConsumerSettings(100, 11.seconds, 2.days),
        KafkaMatcherQueue.ProducerSettings(200)
      )
    )
  }
}

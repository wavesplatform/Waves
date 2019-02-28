package com.wavesplatform.settings

import cats.implicits._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.MatcherSettings.EventsQueueSettings
import com.wavesplatform.matcher.api.OrderBookSnapshotHttpCache
import com.wavesplatform.matcher.queue.{KafkaMatcherQueue, LocalMatcherQueue}
import com.wavesplatform.settings.fee.AssetType
import com.wavesplatform.settings.fee.OrderFeeSettings._
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._
import scala.util.Try

class MatcherSettingsSpecification extends FlatSpec with Matchers {

  def configStrWithOrderFeeSettings(orderFeeSettings: String): Config = {
    val configStr =
      s"""waves {
      |  directory = /waves
      |  matcher {
      |    enable = yes
      |    account = 3Mqjki7bLtMEBRCYeQis39myp9B4cnooDEX
      |    bind-address = 127.0.0.1
      |    port = 6886
      |    order-match-tx-fee = 100000
      |    snapshots-interval = 999
      |    make-snapshots-at-start = yes
      |    snapshots-loading-timeout = 423s
      |    start-events-processing-timeout = 543s
      |    rest-order-limit = 100
      |    order-timestamp-drift = 10m
      |    price-assets = [
      |      WAVES
      |      8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS
      |      DHgwrRvVyqJsepd32YbBqUeDH4GJ1N984X8QoekjgH8J
      |    ]
      |    max-timestamp-diff = 30d
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
      |    $orderFeeSettings
      |  }
      |}""".stripMargin

    loadConfig(ConfigFactory.parseString(configStr))
  }

  "MatcherSettings" should "read values" in {

    val correctOrderFeeSettings =
      s"""
         |order-fee {
         |  mode = fixed
         |  fixed {
         |    asset-id = WAVES
         |    min-fee = 300000
         |  }
         |  percent {
         |    asset-type = amount
         |    min-fee = 0.1
         |  }
         |}
       """.stripMargin

    val config = configStrWithOrderFeeSettings(correctOrderFeeSettings)

    val settings = MatcherSettings.fromConfig(config)
    settings.enable should be(true)
    settings.account should be("3Mqjki7bLtMEBRCYeQis39myp9B4cnooDEX")
    settings.bindAddress should be("127.0.0.1")
    settings.port should be(6886)
    settings.orderMatchTxFee should be(100000)
    settings.journalDataDir should be("/waves/matcher/journal")
    settings.snapshotsDataDir should be("/waves/matcher/snapshots")
    settings.snapshotsInterval should be(999)
    settings.makeSnapshotsAtStart should be(true)
    settings.snapshotsLoadingTimeout should be(423.seconds)
    settings.startEventsProcessingTimeout should be(543.seconds)
    settings.maxOrdersPerRequest should be(100)
    settings.orderTimestampDrift should be(10.minutes.toMillis)
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

    settings.orderFee match {
      case FixedSettings(defaultAssetId, minFee) =>
        defaultAssetId shouldBe None
        minFee shouldBe 300000
      case PercentSettings(assetType, minFee) =>
        assetType shouldBe AssetType.AMOUNT
        minFee shouldBe 0.1
    }
  }

  "MatcherSettings" should "produce errors" in {

    def getSettingByConfig(conf: Config): Either[String, MatcherSettings] = Try(MatcherSettings.fromConfig(conf)).toEither.leftMap(_.getMessage)

    val invalidMode =
      s"""
         |order-fee {
         |  mode = invalid
         |  fixed {
         |    asset-id = WAVES
         |    min-fee = 300000
         |  }
         |  percent {
         |    asset-type = amount
         |    min-fee = 0.1
         |  }
         |}
       """.stripMargin

    val invalidTypeAndPercent =
      s"""
         |order-fee {
         |  mode = percent
         |  fixed {
         |    asset-id = WAVES
         |    min-fee = 300000
         |  }
         |  percent {
         |    asset-type = test
         |    min-fee = 121
         |  }
         |}
       """.stripMargin

    val invalidAsset =
      s"""
         |order-fee {
         |  mode = fixed
         |  fixed {
         |    asset-id = ;;;;
         |    min-fee = -300000
         |  }
         |  percent {
         |    asset-type = test
         |    min-fee = 121
         |  }
         |}
       """.stripMargin

    val settingsInvalidMode           = getSettingByConfig(configStrWithOrderFeeSettings(invalidMode))
    val settingsInvalidTypeAndPercent = getSettingByConfig(configStrWithOrderFeeSettings(invalidTypeAndPercent))
    val settingsInvalidAssetAndFee    = getSettingByConfig(configStrWithOrderFeeSettings(invalidAsset))

    settingsInvalidMode shouldBe Left("Invalid setting waves.matcher.order-fee.mode value: invalid")

    settingsInvalidTypeAndPercent shouldBe
      Left(
        "Invalid setting waves.matcher.order-fee.percent.asset-type value: test\n" +
          "Invalid setting waves.matcher.order-fee.percent.min-fee value: 121.0, required 0 < p <= 100")

    settingsInvalidAssetAndFee shouldBe
      Left(
        "Invalid setting waves.matcher.order-fee.fixed.asset-id value: ;;;;\n" +
          "Invalid setting waves.matcher.order-fee.fixed.min-fee value: -300000, must be > 0")
  }

}

package com.wavesplatform.settings

import cats.implicits._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.MatcherSettings.EventsQueueSettings
import com.wavesplatform.matcher.api.OrderBookSnapshotHttpCache
import com.wavesplatform.matcher.queue.{KafkaMatcherQueue, LocalMatcherQueue}
import com.wavesplatform.settings.OrderFeeSettings._
import com.wavesplatform.state.diffs.produce
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._
import scala.util.Try

class MatcherSettingsSpecification extends FlatSpec with Matchers {

  def getSettingByConfig(conf: Config): Either[String, MatcherSettings] = Try(MatcherSettings.fromConfig(conf)).toEither.leftMap(_.getMessage)

  val correctOrderFeeSettingsStr: String =
    s"""
       |order-fee {
       |  mode = percent
       |  waves {
       |    base-fee = 300000
       |  }
       |  fixed {
       |    asset = WAVES
       |    min-fee = 300000
       |  }
       |  percent {
       |    asset-type = amount
       |    min-fee = 0.1
       |  }
       |}
       """.stripMargin

  val correctDeviationSettingsStr: String =
    s"""
       |max-price-deviations {
       |  enable = yes
       |  profit = 1000000
       |  loss = 1000000
       |  fee = 1000000
       |}
     """.stripMargin

  val correctAllowedAssetPairsSettings: String =
    s"""
       |allowed-asset-pairs = []
     """.stripMargin

  def configStrWithSettings(orderFeeSettingsStr: String)(deviationsSettingsStr: String)(allowedAssetPairsSettingsStr: String): Config = {
    val configStr =
      s"""waves {
      |  directory = /waves
      |  matcher {
      |    enable = yes
      |    account = 3Mqjki7bLtMEBRCYeQis39myp9B4cnooDEX
      |    bind-address = 127.0.0.1
      |    port = 6886
      |    actor-response-timeout = 11s
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
      |    $orderFeeSettingsStr
      |    $deviationsSettingsStr
      |    $allowedAssetPairsSettingsStr
      |  }
      |}""".stripMargin

    loadConfig(ConfigFactory.parseString(configStr))
  }

  "MatcherSettings" should "read values" in {

    val config = configStrWithSettings(correctOrderFeeSettingsStr)(correctDeviationSettingsStr)(correctAllowedAssetPairsSettings)

    val settings = MatcherSettings.fromConfig(config)
    settings.enable should be(true)
    settings.account should be("3Mqjki7bLtMEBRCYeQis39myp9B4cnooDEX")
    settings.bindAddress should be("127.0.0.1")
    settings.port should be(6886)
    settings.actorResponseTimeout should be(11.seconds)
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

    settings.orderFee match {
      case FixedWavesSettings(baseFee) =>
        baseFee shouldBe 300000
      case FixedSettings(defaultAssetId, minFee) =>
        defaultAssetId shouldBe None
        minFee shouldBe 300000
      case PercentSettings(assetType, minFee) =>
        assetType shouldBe AssetType.AMOUNT
        minFee shouldBe 0.1
    }
  }

  "DeviationsSettings in MatcherSettings" should "be validated" in {

    val invalidEnable: String =
      s"""
         |max-price-deviations {
         |  enable = foobar
         |  profit = 1000000
         |  loss = 1000000
         |  fee = 1000000
         |}
     """.stripMargin

    val invalidProfit: String =
      s"""
         |max-price-deviations {
         |  enable = yes
         |  profit = -1000000
         |  loss = 1000000
         |  fee = 1000000
         |}
     """.stripMargin

    val invalidLossAndFee: String =
      s"""
         |max-price-deviations {
         |  enable = yes
         |  profit = 1000000
         |  loss = 0
         |  fee = -1000000
         |}
     """.stripMargin

    val configStr: String => Config = configStrWithSettings(correctOrderFeeSettingsStr)(_)(correctAllowedAssetPairsSettings)
    val settingsInvalidEnable       = getSettingByConfig(configStr(invalidEnable))
    val settingsInvalidProfit       = getSettingByConfig(configStr(invalidProfit))
    val settingsInvalidLossAndFee   = getSettingByConfig(configStr(invalidLossAndFee))

    settingsInvalidEnable shouldBe Left("Invalid setting waves.matcher.max-price-deviations.enable value: foobar")

    settingsInvalidProfit shouldBe
      Left("Invalid setting waves.matcher.max-price-deviations.profit value: -1000000, required 0 < percent")

    settingsInvalidLossAndFee shouldBe
      Left(
        "Invalid setting waves.matcher.max-price-deviations.loss value: 0, required 0 < percent, " +
          "Invalid setting waves.matcher.max-price-deviations.fee value: -1000000, required 0 < percent")
  }

  "OrderFeeSettings in MatcherSettings" should "be validated" in {

    val invalidMode =
      s"""
         |order-fee {
         |  mode = invalid
         |  waves {
         |    base-fee = 300000
         |  }
         |  fixed {
         |    asset = WAVES
         |    min-fee = 300000
         |  }
         |  percent {
         |    asset-type = amount
         |    min-fee = 0.1
         |  }
         |}
       """.stripMargin

    val invalidAssetTypeAndPercent =
      s"""
         |order-fee {
         |  mode = percent
         |  waves {
         |    base-fee = 300000
         |  }
         |  fixed {
         |    asset = WAVES
         |    min-fee = 300000
         |  }
         |  percent {
         |    asset-type = test
         |    min-fee = 121.2
         |  }
         |}
       """.stripMargin

    val invalidAssetAndFee =
      s"""
         |order-fee {
         |  mode = fixed
         |  waves {
         |    base-fee = 300000
         |  }
         |  fixed {
         |    asset = ;;;;
         |    min-fee = -300000
         |  }
         |  percent {
         |    asset-type = test
         |    min-fee = 121
         |  }
         |}
       """.stripMargin

    val invalidFeeInWaves =
      s"""
         |order-fee {
         |  mode = waves
         |  waves {
         |    base-fee = -350000
         |  }
         |  fixed {
         |    asset = ;;;;
         |    min-fee = -300000
         |  }
         |  percent {
         |    asset-type = test
         |    min-fee = 121
         |  }
         |}
       """.stripMargin

    val configStr: String => Config   = configStrWithSettings(_)(correctDeviationSettingsStr)(correctAllowedAssetPairsSettings)
    val settingsInvalidMode           = getSettingByConfig(configStr(invalidMode))
    val settingsInvalidTypeAndPercent = getSettingByConfig(configStr(invalidAssetTypeAndPercent))
    val settingsInvalidAssetAndFee    = getSettingByConfig(configStr(invalidAssetAndFee))
    val settingsInvalidFeeInWaves     = getSettingByConfig(configStr(invalidFeeInWaves))

    settingsInvalidMode shouldBe Left("Invalid setting waves.matcher.order-fee.mode value: invalid")

    settingsInvalidTypeAndPercent shouldBe
      Left(
        "Invalid setting waves.matcher.order-fee.percent.asset-type value: test, " +
          "Invalid setting waves.matcher.order-fee.percent.min-fee value: 121.2, required 0 < percent <= 100")

    settingsInvalidAssetAndFee shouldBe
      Left(
        "Invalid setting waves.matcher.order-fee.fixed.asset value: ;;;;, " +
          "Invalid setting waves.matcher.order-fee.fixed.min-fee value: -300000, required 0 < fee")

    settingsInvalidFeeInWaves shouldBe Left(
      s"Invalid setting waves.matcher.order-fee.waves.base-fee value: -350000, required 0 < base fee <= ${OrderFeeSettings.totalWavesAmount}"
    )
  }

  "AllowedAssetPairsSettings" should "be validated" in {

    val configStr: String => Config = configStrWithSettings(correctOrderFeeSettingsStr)(correctDeviationSettingsStr)

    val incorrectAssetsCount =
      """allowed-asset-pairs = [
        | "WAVES-BTC",
        | "WAVES-BTC-ETH",
        | "ETH"
        |]
      """.stripMargin

    val incorrectAssets =
      """allowed-asset-pairs = [
        | "WAVES-;;;",
        | "WAVES-BTC"
        |]
      """.stripMargin

    val duplicates =
      """allowed-asset-pairs = [
        | "WAVES-BTC",
        | "WAVES-ETH",
        | "WAVES-BTC"
        |]
      """.stripMargin

    getSettingByConfig(configStr(incorrectAssetsCount)) should produce(
      "Invalid setting waves.matcher.allowed-asset-pairs value: [WAVES-BTC, WAVES-BTC-ETH, ETH], Incorrect assets count (expected 2, but got 3): WAVES-BTC-ETH, Incorrect assets count (expected 2, but got 1): ETH"
    )

    getSettingByConfig(configStr(incorrectAssets)) should produce(
      "Invalid setting waves.matcher.allowed-asset-pairs value: [WAVES-;;;, WAVES-BTC], requirement failed: Wrong char ';' in Base58 string ';;;'"
    )

    getSettingByConfig(configStr(duplicates)).explicitGet().allowedAssetPairs.size shouldBe 2
  }
}

package com.wavesplatform.matcher.settings

import cats.implicits._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.matcher.api.OrderBookSnapshotHttpCache
import com.wavesplatform.matcher.queue.{KafkaMatcherQueue, LocalMatcherQueue}
import com.wavesplatform.matcher.settings.MatcherSettings.{EventsQueueSettings, ExchangeTransactionBroadcastSettings}
import com.wavesplatform.matcher.settings.OrderFeeSettings.{FixedSettings, DynamicSettings, PercentSettings}
import com.wavesplatform.settings.loadConfig
import com.wavesplatform.state.diffs.produce
import com.wavesplatform.transaction.assets.exchange.AssetPair
import net.ceedubs.ficus.Ficus._
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._
import scala.util.Try

class MatcherSettingsSpecification extends FlatSpec with Matchers {

  def getSettingByConfig(conf: Config): Either[String, MatcherSettings] =
    Try(conf.as[MatcherSettings]("waves.matcher")).toEither.leftMap(_.getMessage)

  val correctOrderFeeStr: String =
    s"""
       |order-fee {
       |  mode = percent
       |  dynamic {
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

  val correctDeviationsStr: String =
    s"""
       |max-price-deviations {
       |  enable = yes
       |  profit = 1000000
       |  loss = 1000000
       |  fee = 1000000
       |}
     """.stripMargin

  val correctAllowedAssetPairsStr: String =
    s"""
       |allowed-asset-pairs = []
     """.stripMargin

  val correctOrderRestrictionsStr: String =
    s"""
       |order-restrictions = []
     """.stripMargin

  def configWithSettings(orderFeeStr: String)(deviationsStr: String)(allowedAssetPairsStr: String)(orderRestrictionsStr: String): Config = {
    val configStr =
      s"""waves {
      |  directory = /waves
      |  matcher {
      |    account = 3Mqjki7bLtMEBRCYeQis39myp9B4cnooDEX
      |    bind-address = 127.0.0.1
      |    port = 6886
      |    exchange-tx-base-fee = 300000
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
      |        enable-storing = no
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
      |        producer {
      |          enable = no
      |          buffer-size = 200
      |        }
      |      }
      |    }
      |    $orderFeeStr
      |    $deviationsStr
      |    $allowedAssetPairsStr
      |    allow-order-v3 = no
      |    $orderRestrictionsStr
      |    exchange-transaction-broadcast {
      |      broadcast-until-confirmed = yes
      |      interval = 1 day
      |      max-pending-time = 30 days
      |    }
      |  }
      |}""".stripMargin

    loadConfig(ConfigFactory.parseString(configStr))
  }

  "MatcherSettings" should "read values" in {

    val config = configWithSettings(correctOrderFeeStr)(correctDeviationsStr)(correctAllowedAssetPairsStr)(correctOrderRestrictionsStr)

    val settings = config.as[MatcherSettings]("waves.matcher")
    settings.account should be("3Mqjki7bLtMEBRCYeQis39myp9B4cnooDEX")
    settings.bindAddress should be("127.0.0.1")
    settings.port should be(6886)
    settings.exchangeTxBaseFee should be(300000)
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
      local = LocalMatcherQueue.Settings(enableStoring = false, 1.day, 99, cleanBeforeConsume = false),
      kafka = KafkaMatcherQueue.Settings(
        "some-events",
        KafkaMatcherQueue.ConsumerSettings(100, 11.seconds, 2.days),
        KafkaMatcherQueue.ProducerSettings(enable = false, 200)
      )
    )

    settings.orderFee match {
      case DynamicSettings(baseFee) =>
        baseFee shouldBe 300000
      case FixedSettings(defaultAssetId, minFee) =>
        defaultAssetId shouldBe None
        minFee shouldBe 300000
      case PercentSettings(assetType, minFee) =>
        assetType shouldBe AssetType.AMOUNT
        minFee shouldBe 0.1
    }

    settings.deviation shouldBe DeviationsSettings(true, 1000000, 1000000, 1000000)
    settings.allowedAssetPairs shouldBe Set.empty[AssetPair]
    settings.allowOrderV3 shouldBe false
    settings.orderRestrictions shouldBe Map.empty[AssetPair, OrderRestrictionsSettings]
    settings.exchangeTransactionBroadcast shouldBe ExchangeTransactionBroadcastSettings(
      broadcastUntilConfirmed = true,
      interval = 1.day,
      maxPendingTime = 30.days
    )
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

    val configStr: String => Config = configWithSettings(correctOrderFeeStr)(_)(correctAllowedAssetPairsStr)(correctOrderRestrictionsStr)
    val settingsInvalidEnable       = getSettingByConfig(configStr(invalidEnable))
    val settingsInvalidProfit       = getSettingByConfig(configStr(invalidProfit))
    val settingsInvalidLossAndFee   = getSettingByConfig(configStr(invalidLossAndFee))

    settingsInvalidEnable shouldBe Left("Invalid setting max-price-deviations.enable value: foobar")

    settingsInvalidProfit shouldBe
      Left("Invalid setting max-price-deviations.profit value: -1000000 (required 0 < percent)")

    settingsInvalidLossAndFee shouldBe
      Left(
        "Invalid setting max-price-deviations.loss value: 0 (required 0 < percent), " +
          "Invalid setting max-price-deviations.fee value: -1000000 (required 0 < percent)")
  }

  "OrderFeeSettings in MatcherSettings" should "be validated" in {

    val invalidMode =
      s"""
         |order-fee {
         |  mode = invalid
         |  dynamic {
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
         |  dynamic {
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
         |  dynamic {
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

    val invalidFeeInDynamicMode =
      s"""
         |order-fee {
         |  mode = dynamic
         |  dynamic {
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

    val configStr: String => Config     = configWithSettings(_)(correctDeviationsStr)(correctAllowedAssetPairsStr)(correctOrderRestrictionsStr)
    val settingsInvalidMode             = getSettingByConfig(configStr(invalidMode))
    val settingsInvalidTypeAndPercent   = getSettingByConfig(configStr(invalidAssetTypeAndPercent))
    val settingsInvalidAssetAndFee      = getSettingByConfig(configStr(invalidAssetAndFee))
    val settingsInvalidFeeInDynamicMode = getSettingByConfig(configStr(invalidFeeInDynamicMode))

    settingsInvalidMode shouldBe Left("Invalid setting order-fee.mode value: invalid")

    settingsInvalidTypeAndPercent shouldBe
      Left(
        "Invalid setting order-fee.percent.asset-type value: test, " +
          "Invalid setting order-fee.percent.min-fee value: 121.2 (required 0 < percent <= 100)")

    settingsInvalidAssetAndFee shouldBe
      Left(
        "Invalid setting order-fee.fixed.asset value: ;;;;, " +
          "Invalid setting order-fee.fixed.min-fee value: -300000 (required 0 < fee)")

    settingsInvalidFeeInDynamicMode shouldBe Left(
      s"Invalid setting order-fee.dynamic.base-fee value: -350000 (required 0 < base fee <= ${OrderFeeSettings.totalWavesAmount})"
    )
  }

  "Allowed asset pairs in MatcherSettings" should "be validated" in {

    val configStr: String => Config = configWithSettings(correctOrderFeeStr)(correctDeviationsStr)(_)(correctOrderRestrictionsStr)

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

    val nonEmptyCorrect =
      """allowed-asset-pairs = [
        | "WAVES-BTC",
        | "WAVES-ETH",
        | "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS-WAVES"
        |]
      """.stripMargin

    getSettingByConfig(configStr(incorrectAssetsCount)) should produce(
      "Invalid setting allowed-asset-pairs value: WAVES-BTC-ETH (incorrect assets count, expected 2 but got 3), ETH (incorrect assets count, expected 2 but got 1)"
    )

    getSettingByConfig(configStr(incorrectAssets)) should produce(
      "Invalid setting allowed-asset-pairs value: WAVES-;;; (requirement failed: Wrong char ';' in Base58 string ';;;')"
    )

    getSettingByConfig(configStr(duplicates)).explicitGet().allowedAssetPairs.size shouldBe 2

    getSettingByConfig(configStr(nonEmptyCorrect)).explicitGet().allowedAssetPairs shouldBe
      Set(
        AssetPair.createAssetPair("WAVES", "BTC").get,
        AssetPair.createAssetPair("WAVES", "ETH").get,
        AssetPair.createAssetPair("8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS", "WAVES").get
      )
  }

  "Order restrictions" should "be validated" in {

    val configStr: String => Config = configWithSettings(correctOrderFeeStr)(correctDeviationsStr)(correctAllowedAssetPairsStr)(_)

    val defaults =
      """order-restrictions = [
        | {
        |   pair = "WAVES-BTC"
        | },
        | {
        |   pair = "ETH-WAVES"
        | }
        |]
      """.stripMargin

    val nonEmptyCorrect =
      """order-restrictions = [
        | {
        |   pair = "WAVES-BTC",
        |   step-size = 0.001,
        |   min-amount = 0.001,
        |   max-amount = 1000000
        | },
        | {
        |   pair = "ETH-USD",
        |   step-size = 0.01,
        |   min-amount = 0.05,
        |   max-amount = 20000,
        |   merge-small-prices = yes
        | }
        |]
      """.stripMargin

    val incorrectPairAndStepSize =
      """order-restrictions = [
        | {
        |   pair = "WAVES-BTC",
        |   step-size = -0.013,
        |   min-amount = 0.001,
        |   max-amount = 1000000
        | },
        | {
        |   pair = "ETH-;;;",
        |   step-size = 0.01,
        |   min-amount = 0.05,
        |   max-amount = 20000
        | }
        |]
      """.stripMargin

    val incorrectMinAndMax =
      """order-restrictions = [
        | {
        |   pair = "WAVES-BTC",
        |   step-size = 0.013,
        |   min-amount = 0.001,
        |   max-amount = 1000000,
        |   min-price = 100
        |   max-price = 10
        | },
        | {
        |   pair = "ETH-WAVES",
        |   tick-size = 0.14,
        |   max-price = 17
        | }
        |]
      """.stripMargin

    val defaultOrderRestrictionsSettings =
      OrderRestrictionsSettings(
        stepSize = OrderRestrictionsSettings.stepSizeDefault,
        minAmount = OrderRestrictionsSettings.minAmountDefault,
        maxAmount = OrderRestrictionsSettings.maxAmountDefault,
        tickSize = OrderRestrictionsSettings.tickSizeDefault,
        minPrice = OrderRestrictionsSettings.minPriceDefault,
        maxPrice = OrderRestrictionsSettings.maxPriceDefault,
        mergeSmallPrices = false
      )

    getSettingByConfig(configStr(defaults)).explicitGet().orderRestrictions shouldBe Map(
      AssetPair.createAssetPair("WAVES", "BTC").get -> defaultOrderRestrictionsSettings,
      AssetPair.createAssetPair("ETH", "WAVES").get -> defaultOrderRestrictionsSettings
    )

    getSettingByConfig(configStr(nonEmptyCorrect)).explicitGet().orderRestrictions shouldBe
      Map(
        AssetPair.createAssetPair("WAVES", "BTC").get ->
          OrderRestrictionsSettings(
            stepSize = 0.001,
            minAmount = 0.001,
            maxAmount = 1000000,
            tickSize = OrderRestrictionsSettings.tickSizeDefault,
            minPrice = OrderRestrictionsSettings.minPriceDefault,
            maxPrice = OrderRestrictionsSettings.maxPriceDefault,
            mergeSmallPrices = false
          ),
        AssetPair.createAssetPair("ETH", "USD").get ->
          OrderRestrictionsSettings(
            stepSize = 0.01,
            minAmount = 0.05,
            maxAmount = 20000,
            tickSize = OrderRestrictionsSettings.tickSizeDefault,
            minPrice = OrderRestrictionsSettings.minPriceDefault,
            maxPrice = OrderRestrictionsSettings.maxPriceDefault,
            mergeSmallPrices = true
          )
      )

    getSettingByConfig(configStr(incorrectPairAndStepSize)) should produce(
      "Invalid setting order-restrictions.0.step-size value: -0.013 (required 0 < value), " +
        "Invalid setting order-restrictions.1.pair value: ETH-;;;"
    )

    getSettingByConfig(configStr(incorrectMinAndMax)) should produce("Required order-restrictions.0.min-price < order-restrictions.0.max-price")
  }
}

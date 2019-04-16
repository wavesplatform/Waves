package com.wavesplatform.matcher

import java.io.File

import com.typesafe.config.Config
import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.matcher.MatcherSettings.{ExchangeTransactionBroadcastSettings, EventsQueueSettings}
import com.wavesplatform.matcher.api.OrderBookSnapshotHttpCache
import com.wavesplatform.matcher.queue.{KafkaMatcherQueue, LocalMatcherQueue}
import com.wavesplatform.transaction.assets.exchange.AssetPair
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader.arbitraryTypeValueReader
import net.ceedubs.ficus.readers.{NameMapper, ValueReader}

import scala.concurrent.duration.FiniteDuration
import scala.util.matching.Regex

case class MatcherSettings(enable: Boolean,
                           account: String,
                           bindAddress: String,
                           port: Int,
                           minOrderFee: Long,
                           orderMatchTxFee: Long,
                           dataDir: String,
                           recoverOrderHistory: Boolean,
                           journalDataDir: String,
                           snapshotsDataDir: String,
                           snapshotsInterval: Int,
                           snapshotsLoadingTimeout: FiniteDuration,
                           startEventsProcessingTimeout: FiniteDuration,
                           makeSnapshotsAtStart: Boolean,
                           priceAssets: Seq[String],
                           blacklistedAssets: Set[String],
                           blacklistedNames: Seq[Regex],
                           maxOrdersPerRequest: Int,
                           // this is not a Set[Address] because to parse an address, global AddressScheme must be initialized
                           blacklistedAddresses: Set[String],
                           orderBookSnapshotHttpCache: OrderBookSnapshotHttpCache.Settings,
                           balanceWatchingBufferInterval: FiniteDuration,
                           eventsQueue: EventsQueueSettings,
                           disableExtraFeeForScript: Boolean,
                           allowedAssetPairs: Set[AssetPair],
                           exchangeTransactionBroadcast: ExchangeTransactionBroadcastSettings)

object MatcherSettings {
  implicit val addressReader: ValueReader[Address] = (cfg, path) => Address.fromString(cfg.getString(path)).explicitGet()
  implicit val chosenCase: NameMapper              = net.ceedubs.ficus.readers.namemappers.implicits.hyphenCase

  case class EventsQueueSettings(tpe: String, local: LocalMatcherQueue.Settings, kafka: KafkaMatcherQueue.Settings)
  private[this] implicit val eventsQueueSettingsReader: ValueReader[EventsQueueSettings] = { (cfg, path) =>
    EventsQueueSettings(
      tpe = cfg.getString(s"$path.type"),
      local = cfg.as[LocalMatcherQueue.Settings](s"$path.local"),
      kafka = cfg.as[KafkaMatcherQueue.Settings](s"$path.kafka")
    )
  }

  implicit val matcherSettingsValueReader: ValueReader[MatcherSettings] =
    (cfg, path) => fromConfig(cfg.getConfig(path))

  // @deprecated("Use config.as[MatcherSettings]", "0.17.0")
  def fromRootConfig(config: Config): MatcherSettings = config.as[MatcherSettings]("waves.matcher")

  private[this] def fromConfig(config: Config): MatcherSettings = {
    val enabled                      = config.as[Boolean]("enable")
    val account                      = config.as[String]("account")
    val bindAddress                  = config.as[String]("bind-address")
    val port                         = config.as[Int]("port")
    val minOrderFee                  = config.as[Long]("min-order-fee")
    val orderMatchTxFee              = config.as[Long]("order-match-tx-fee")
    val dataDirectory                = config.as[String]("data-directory")
    val journalDirectory             = config.as[String]("journal-directory")
    val snapshotsDirectory           = config.as[String]("snapshots-directory")
    val snapshotsInterval            = config.as[Int]("snapshots-interval")
    val snapshotsLoadingTimeout      = config.as[FiniteDuration]("snapshots-loading-timeout")
    val startEventsProcessingTimeout = config.as[FiniteDuration]("start-events-processing-timeout")
    val makeSnapshotsAtStart         = config.as[Boolean]("make-snapshots-at-start")
    val maxOrdersPerRequest          = config.as[Int]("rest-order-limit")
    val baseAssets                   = config.as[List[String]]("price-assets")

    val blacklistedAssets = config.as[List[String]]("blacklisted-assets")
    val blacklistedNames  = config.as[List[String]]("blacklisted-names").map(_.r)

    val blacklistedAddresses       = config.as[Set[String]]("blacklisted-addresses")
    val orderBookSnapshotHttpCache = config.as[OrderBookSnapshotHttpCache.Settings]("order-book-snapshot-http-cache")

    val balanceWatchingBufferInterval = config.as[FiniteDuration]("balance-watching-buffer-interval")

    val eventsQueue         = config.as[EventsQueueSettings](s"events-queue")
    val recoverOrderHistory = !new File(dataDirectory).exists()

    def getAssetPairFromString(source: String): AssetPair = {
      val sourceArr = source.split("-")
      val res = sourceArr match {
        case Array(amtAssetStr, prcAssetStr) => AssetPair.createAssetPair(amtAssetStr, prcAssetStr)
        case _                               => throw new Exception(s"Incorrect assets count (expected 2, but got ${sourceArr.size}): $source")
      }
      res fold (ex => throw new Exception(ex.getMessage), identity)
    }

    val allowedAssetPairs        = config.as[Set[String]]("allowed-asset-pairs").map(getAssetPairFromString)
    val disableExtraFeeForScript = config.as[Boolean]("disable-extra-fee-for-script")
    val broadcastUntilConfirmed  = config.as[ExchangeTransactionBroadcastSettings]("exchange-transaction-broadcast")

    MatcherSettings(
      enabled,
      account,
      bindAddress,
      port,
      minOrderFee,
      orderMatchTxFee,
      dataDirectory,
      recoverOrderHistory,
      journalDirectory,
      snapshotsDirectory,
      snapshotsInterval,
      snapshotsLoadingTimeout,
      startEventsProcessingTimeout,
      makeSnapshotsAtStart,
      baseAssets,
      blacklistedAssets.toSet,
      blacklistedNames,
      maxOrdersPerRequest,
      blacklistedAddresses,
      orderBookSnapshotHttpCache,
      balanceWatchingBufferInterval,
      eventsQueue,
      disableExtraFeeForScript,
      allowedAssetPairs,
      broadcastUntilConfirmed
    )
  }

  case class ExchangeTransactionBroadcastSettings(broadcastUntilConfirmed: Boolean, interval: FiniteDuration, maxPendingTime: FiniteDuration)
}

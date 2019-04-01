package com.wavesplatform.matcher.settings

import java.io.File

import com.typesafe.config.Config
import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.matcher.api.OrderBookSnapshotHttpCache
import com.wavesplatform.matcher.queue.{KafkaMatcherQueue, LocalMatcherQueue}
import com.wavesplatform.matcher.settings.MatcherSettings.EventsQueueSettings
import com.wavesplatform.settings.DeviationsSettings._
import com.wavesplatform.settings.OrderAmountSettings._
import com.wavesplatform.settings.OrderFeeSettings._
import com.wavesplatform.settings.utils.ConfigOps._
import com.wavesplatform.settings.{DeviationsSettings, OrderAmountSettings}
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.transaction.assets.exchange.AssetPair._
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader.arbitraryTypeValueReader
import net.ceedubs.ficus.readers.{NameMapper, ValueReader}

import scala.concurrent.duration.FiniteDuration
import scala.util.matching.Regex

case class MatcherSettings(account: String,
                           bindAddress: String,
                           port: Int,
                           actorResponseTimeout: FiniteDuration,
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
                           orderFee: OrderFeeSettings,
                           deviation: DeviationsSettings,
                           orderAmountRestrictions: Map[AssetPair, OrderAmountSettings],
                           allowedAssetPairs: Set[AssetPair],
                           allowOrderV3: Boolean)

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

  private[this] def fromConfig(config: Config): MatcherSettings = {
    val account                      = config.as[String]("account")
    val bindAddress                  = config.as[String]("bind-address")
    val port                         = config.as[Int]("port")
    val actorResponseTimeout         = config.as[FiniteDuration]("actor-response-timeout")
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

    val orderFee                = config.as[OrderFeeSettings]("order-fee")
    val deviation               = config.as[DeviationsSettings]("max-price-deviations")
    val orderAmountRestrictions = config.getFailSlowSetOf[(AssetPair, OrderAmountSettings)]("order-amount-restrictions").toMap
    val allowedAssetPairs       = config.getFailSlowSetOf[AssetPair]("allowed-asset-pairs")
    val allowOrderV3            = config.as[Boolean]("allow-order-v3")

    MatcherSettings(
      account,
      bindAddress,
      port,
      actorResponseTimeout,
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
      orderFee,
      deviation,
      orderAmountRestrictions,
      allowedAssetPairs,
      allowOrderV3
    )
  }
}

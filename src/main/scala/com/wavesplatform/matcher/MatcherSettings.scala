package com.wavesplatform.matcher

import java.io.File

import com.typesafe.config.Config
import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.matcher.MatcherSettings.EventsQueueSettings
import com.wavesplatform.matcher.api.OrderBookSnapshotHttpCache
import com.wavesplatform.matcher.queue.{KafkaMatcherQueue, LocalMatcherQueue}
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
                           eventsQueue: EventsQueueSettings)

object MatcherSettings {

  implicit val addressReader: ValueReader[Address] = (cfg, path) => Address.fromString(cfg.getString(path)).explicitGet()

  implicit val chosenCase: NameMapper = net.ceedubs.ficus.readers.namemappers.implicits.hyphenCase
  val configPath: String              = "waves.matcher"

  case class EventsQueueSettings(tpe: String, local: LocalMatcherQueue.Settings, kafka: KafkaMatcherQueue.Settings)
  private implicit val reader: ValueReader[EventsQueueSettings] = { (cfg, path) =>
    EventsQueueSettings(
      tpe = cfg.getString(s"$path.type"),
      local = cfg.as[LocalMatcherQueue.Settings](s"$path.local"),
      kafka = cfg.as[KafkaMatcherQueue.Settings](s"$path.kafka")
    )
  }

  def fromConfig(config: Config): MatcherSettings = {
    val enabled                      = config.as[Boolean](s"$configPath.enable")
    val account                      = config.as[String](s"$configPath.account")
    val bindAddress                  = config.as[String](s"$configPath.bind-address")
    val port                         = config.as[Int](s"$configPath.port")
    val minOrderFee                  = config.as[Long](s"$configPath.min-order-fee")
    val orderMatchTxFee              = config.as[Long](s"$configPath.order-match-tx-fee")
    val dataDirectory                = config.as[String](s"$configPath.data-directory")
    val journalDirectory             = config.as[String](s"$configPath.journal-directory")
    val snapshotsDirectory           = config.as[String](s"$configPath.snapshots-directory")
    val snapshotsInterval            = config.as[Int](s"$configPath.snapshots-interval")
    val snapshotsLoadingTimeout      = config.as[FiniteDuration](s"$configPath.snapshots-loading-timeout")
    val startEventsProcessingTimeout = config.as[FiniteDuration](s"$configPath.start-events-processing-timeout")
    val makeSnapshotsAtStart         = config.as[Boolean](s"$configPath.make-snapshots-at-start")
    val maxOrdersPerRequest          = config.as[Int](s"$configPath.rest-order-limit")
    val baseAssets                   = config.as[List[String]](s"$configPath.price-assets")

    val blacklistedAssets = config.as[List[String]](s"$configPath.blacklisted-assets")
    val blacklistedNames  = config.as[List[String]](s"$configPath.blacklisted-names").map(_.r)

    val blacklistedAddresses       = config.as[Set[String]](s"$configPath.blacklisted-addresses")
    val orderBookSnapshotHttpCache = config.as[OrderBookSnapshotHttpCache.Settings](s"$configPath.order-book-snapshot-http-cache")

    val balanceWatchingBufferInterval = config.as[FiniteDuration](s"$configPath.balance-watching-buffer-interval")

    val eventsQueue         = config.as[EventsQueueSettings](s"$configPath.events-queue")
    val recoverOrderHistory = !new File(dataDirectory).exists()

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
      eventsQueue
    )
  }
}

package com.wavesplatform.matcher.settings

import java.io.File

import cats.data.{NonEmptyList, Validated}
import com.typesafe.config.Config
import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.matcher.api.OrderBookSnapshotHttpCache
import com.wavesplatform.matcher.model.OrderValidator
import com.wavesplatform.matcher.queue.{KafkaMatcherQueue, LocalMatcherQueue}
import com.wavesplatform.matcher.settings.DeviationsSettings._
import com.wavesplatform.matcher.settings.MatcherSettings.{EventsQueueSettings, ExchangeTransactionBroadcastSettings, RawMatchingRules}
import com.wavesplatform.matcher.settings.OrderFeeSettings.{OrderFeeSettings, _}
import com.wavesplatform.matcher.settings.OrderHistorySettings._
import com.wavesplatform.matcher.settings.OrderRestrictionsSettings.orderRestrictionsSettingsReader
import com.wavesplatform.matcher.settings.PostgresConnection._
import com.wavesplatform.settings.utils.ConfigOps._
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
                           exchangeTxBaseFee: Long,
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
                           orderRestrictions: Map[AssetPair, OrderRestrictionsSettings],
                           matchingRules: Map[AssetPair, NonEmptyList[RawMatchingRules]],
                           allowedAssetPairs: Set[AssetPair],
                           allowOrderV3: Boolean,
                           exchangeTransactionBroadcast: ExchangeTransactionBroadcastSettings,
                           postgresConnection: PostgresConnection,
                           orderHistory: Option[OrderHistorySettings])

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

  private implicit def nonEmptyListReader[T: ValueReader]: ValueReader[NonEmptyList[T]] = implicitly[ValueReader[List[T]]].map {
    case Nil     => throw new IllegalArgumentException("Expected at least one element")
    case x :: xs => NonEmptyList(x, xs)
  }

  private val rawMatchingRulesNel: ValueReader[NonEmptyList[RawMatchingRules]] = nonEmptyListReader[RawMatchingRules].map { xs =>
    val isStrictOrder = xs.tail.zip(xs.toList).forall { case (next, prev) => next.startOffset > prev.startOffset }
    if (isStrictOrder) { if (xs.head.startOffset != 0) RawMatchingRules(0, false) :: xs else xs } else
      throw new IllegalArgumentException(s"Rules should be ordered by offset, but they are: ${xs.map(_.startOffset).toList.mkString(", ")}")
  }

  implicit val valueReader: ValueReader[MatcherSettings] = (cfg, path) => fromConfig(cfg getConfig path)

  private def parseAssetPair(key: String): Validated[String, AssetPair] =
    Validated.fromTry(AssetPair.fromString(key)).leftMap(_ => s"Can't parse asset pair '$key'")

  private[this] def fromConfig(config: Config): MatcherSettings = {

    val account     = config.as[String]("account")
    val bindAddress = config.as[String]("bind-address")
    val port        = config.as[Int]("port")

    val exchangeTxBaseFee = config.getValidatedByPredicate[Long]("exchange-tx-base-fee")(
      predicate = _ >= OrderValidator.exchangeTransactionCreationFee,
      errorMsg = s"base fee must be >= ${OrderValidator.exchangeTransactionCreationFee}"
    )

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

    val orderFee          = config.as[OrderFeeSettings]("order-fee")
    val deviation         = config.as[DeviationsSettings]("max-price-deviations")
    val orderRestrictions = config.getValidatedMap[AssetPair, OrderRestrictionsSettings]("order-restrictions")(parseAssetPair)
    val matchingRules     = config.getValidatedMap[AssetPair, NonEmptyList[RawMatchingRules]]("matching-rules")(parseAssetPair)(rawMatchingRulesNel)
    val allowedAssetPairs = config.getValidatedSet[AssetPair]("allowed-asset-pairs")

    val allowOrderV3            = config.as[Boolean]("allow-order-v3")
    val broadcastUntilConfirmed = config.as[ExchangeTransactionBroadcastSettings]("exchange-transaction-broadcast")

    val postgresConnection = config.as[PostgresConnection]("postgres")
    val orderHistory       = config.as[Option[OrderHistorySettings]]("order-history")

    MatcherSettings(
      account,
      bindAddress,
      port,
      exchangeTxBaseFee,
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
      orderRestrictions,
      matchingRules,
      allowedAssetPairs,
      allowOrderV3,
      broadcastUntilConfirmed,
      postgresConnection,
      orderHistory
    )
  }

  case class ExchangeTransactionBroadcastSettings(broadcastUntilConfirmed: Boolean, interval: FiniteDuration, maxPendingTime: FiniteDuration)
  case class RawMatchingRules(startOffset: Long, mergePrices: Boolean, tickSize: Double = 0)
}

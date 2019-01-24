package com.wavesplatform.matcher

import java.io.File

import com.typesafe.config.Config
import com.wavesplatform.account.Address
import com.wavesplatform.matcher.api.OrderBookSnapshotHttpCache
import com.wavesplatform.common.utils.EitherExt2
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
                           makeSnapshotsAtStart: Boolean,
                           orderCleanupInterval: FiniteDuration,
                           priceAssets: Seq[String],
                           maxTimestampDiff: FiniteDuration,
                           blacklistedAssets: Set[String],
                           blacklistedNames: Seq[Regex],
                           maxOrdersPerRequest: Int,
                           defaultOrderTimestamp: Long,
                           orderTimestampDrift: Long,
                           // this is not a Set[Address] because to parse an address, global AddressScheme must be initialized
                           blacklistedAddresses: Set[String],
                           orderBookSnapshotHttpCache: OrderBookSnapshotHttpCache.Settings)

object MatcherSettings {

  implicit val addressReader: ValueReader[Address] = (cfg, path) => Address.fromString(cfg.getString(path)).explicitGet()

  implicit val chosenCase: NameMapper = net.ceedubs.ficus.readers.namemappers.implicits.hyphenCase
  val configPath: String              = "waves.matcher"

  def fromConfig(config: Config): MatcherSettings = {
    val enabled               = config.as[Boolean](s"$configPath.enable")
    val account               = config.as[String](s"$configPath.account")
    val bindAddress           = config.as[String](s"$configPath.bind-address")
    val port                  = config.as[Int](s"$configPath.port")
    val minOrderFee           = config.as[Long](s"$configPath.min-order-fee")
    val orderMatchTxFee       = config.as[Long](s"$configPath.order-match-tx-fee")
    val dataDirectory         = config.as[String](s"$configPath.data-directory")
    val journalDirectory      = config.as[String](s"$configPath.journal-directory")
    val snapshotsDirectory    = config.as[String](s"$configPath.snapshots-directory")
    val snapshotsInterval     = config.as[Int](s"$configPath.snapshots-interval")
    val makeSnapshotsAtStart  = config.as[Boolean](s"$configPath.make-snapshots-at-start")
    val orderCleanupInterval  = config.as[FiniteDuration](s"$configPath.order-cleanup-interval")
    val maxOrdersPerRequest   = config.as[Int](s"$configPath.rest-order-limit")
    val defaultOrderTimestamp = config.as[Long](s"$configPath.default-order-timestamp")
    val orderTimestampDrift   = config.as[FiniteDuration](s"$configPath.order-timestamp-drift")
    val baseAssets            = config.as[List[String]](s"$configPath.price-assets")
    val maxTimestampDiff      = config.as[FiniteDuration](s"$configPath.max-timestamp-diff")

    val blacklistedAssets = config.as[List[String]](s"$configPath.blacklisted-assets")
    val blacklistedNames  = config.as[List[String]](s"$configPath.blacklisted-names").map(_.r)

    val blacklistedAddresses       = config.as[Set[String]](s"$configPath.blacklisted-addresses")
    val orderBookSnapshotHttpCache = config.as[OrderBookSnapshotHttpCache.Settings](s"$configPath.order-book-snapshot-http-cache")

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
      makeSnapshotsAtStart,
      orderCleanupInterval,
      baseAssets,
      maxTimestampDiff,
      blacklistedAssets.toSet,
      blacklistedNames,
      maxOrdersPerRequest,
      defaultOrderTimestamp,
      orderTimestampDrift.toMillis,
      blacklistedAddresses,
      orderBookSnapshotHttpCache
    )
  }
}

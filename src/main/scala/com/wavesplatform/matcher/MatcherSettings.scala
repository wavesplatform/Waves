package com.wavesplatform.matcher

import java.io.File

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.NameMapper

import scala.concurrent.duration.FiniteDuration
import scala.util.matching.Regex

case class MatcherSettings(enable: Boolean,
                           account: String,
                           bindAddress: String,
                           port: Int,
                           minOrderFee: Long,
                           orderMatchTxFee: Long,
                           dataDir: String,
                           isMigrateToNewOrderHistoryStorage: Boolean,
                           journalDataDir: String,
                           snapshotsDataDir: String,
                           snapshotsInterval: FiniteDuration,
                           orderCleanupInterval: FiniteDuration,
                           priceAssets: Seq[String],
                           maxTimestampDiff: FiniteDuration,
                           blacklistedAssets: Set[String],
                           blacklistedNames: Seq[Regex],
                           validationTimeout: FiniteDuration,
                           maxOrdersPerRequest: Int,
                           blacklistedAddresses: Set[String])

object MatcherSettings {

  implicit val chosenCase: NameMapper = net.ceedubs.ficus.readers.namemappers.implicits.hyphenCase
  val configPath: String              = "waves.matcher"

  def fromConfig(config: Config): MatcherSettings = {
    val enabled              = config.as[Boolean](s"$configPath.enable")
    val account              = config.as[String](s"$configPath.account")
    val bindAddress          = config.as[String](s"$configPath.bind-address")
    val port                 = config.as[Int](s"$configPath.port")
    val minOrderFee          = config.as[Long](s"$configPath.min-order-fee")
    val orderMatchTxFee      = config.as[Long](s"$configPath.order-match-tx-fee")
    val dataDirectory        = config.as[String](s"$configPath.data-directory")
    val journalDirectory     = config.as[String](s"$configPath.journal-directory")
    val snapshotsDirectory   = config.as[String](s"$configPath.snapshots-directory")
    val snapshotsInterval    = config.as[FiniteDuration](s"$configPath.snapshots-interval")
    val orderCleanupInterval = config.as[FiniteDuration](s"$configPath.order-cleanup-interval")
    val maxOrdersPerRequest  = config.as[Int](s"$configPath.rest-order-limit")
    val baseAssets           = config.as[List[String]](s"$configPath.price-assets")
    val maxTimestampDiff     = config.as[FiniteDuration](s"$configPath.max-timestamp-diff")

    val blacklistedAssets = config.as[List[String]](s"$configPath.blacklisted-assets")
    val validationTimeout = config.as[FiniteDuration](s"$configPath.validation-timeout")
    val blacklistedNames  = config.as[List[String]](s"$configPath.blacklisted-names").map(_.r)

    val blacklistedAddresses = config.as[List[String]](s"$configPath.blacklisted-addresses")

    val isMigrateToNewOrderHistoryStorage = !new File(dataDirectory).exists()

    MatcherSettings(
      enabled,
      account,
      bindAddress,
      port,
      minOrderFee,
      orderMatchTxFee,
      dataDirectory,
      isMigrateToNewOrderHistoryStorage,
      journalDirectory,
      snapshotsDirectory,
      snapshotsInterval,
      orderCleanupInterval,
      baseAssets,
      maxTimestampDiff,
      blacklistedAssets.toSet,
      blacklistedNames,
      validationTimeout,
      maxOrdersPerRequest,
      blacklistedAddresses.toSet,
    )
  }
}

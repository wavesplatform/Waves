package com.wavesplatform.matcher

import java.io.File

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import scorex.transaction.assets.exchange.AssetPair

import scala.collection.JavaConverters._
import scala.concurrent.duration.FiniteDuration
import scala.util.matching.Regex

case class MatcherSettings(enable: Boolean,
                           account: String,
                           bindAddress: String,
                           port: Int,
                           minOrderFee: Long,
                           orderMatchTxFee: Long,
                           journalDataDir: String,
                           snapshotsDataDir: String,
                           snapshotsInterval: FiniteDuration,
                           orderCleanupInterval: FiniteDuration,
                           orderHistoryCommitInterval: FiniteDuration,
                           maxOpenOrders: Int,
                           priceAssets: Seq[String],
                           predefinedPairs: Seq[AssetPair],
                           maxTimestampDiff: FiniteDuration,
                           orderHistoryFile: Option[File],
                           isMigrateToNewOrderHistoryStorage: Boolean,
                           blacklistedAssets: Set[String],
                           blacklistedNames: Seq[Regex],
                           txHistoryFile: Option[File],
                           maxOrdersPerRequest: Int,
                           blacklistedAdresses: Set[String],
                           maxOrdersPerAddress: Int,
                          )


object MatcherSettings {
  val configPath: String = "waves.matcher"
  import com.wavesplatform.settings.fileReader

  def fromConfig(config: Config): MatcherSettings = {
    val enabled = config.as[Boolean](s"$configPath.enable")
    val account = config.as[String](s"$configPath.account")
    val bindAddress = config.as[String](s"$configPath.bind-address")
    val port = config.as[Int](s"$configPath.port")
    val minOrderFee = config.as[Long](s"$configPath.min-order-fee")
    val orderMatchTxFee = config.as[Long](s"$configPath.order-match-tx-fee")
    val journalDirectory = config.as[String](s"$configPath.journal-directory")
    val snapshotsDirectory = config.as[String](s"$configPath.snapshots-directory")
    val snapshotsInterval = config.as[FiniteDuration](s"$configPath.snapshots-interval")
    val orderCleanupInterval = config.as[FiniteDuration](s"$configPath.order-cleanup-interval")
    val orderHistoryCommitInterval = config.as[FiniteDuration](s"$configPath.order-history-commit-interval")
    val maxOpenOrders = config.as[Int](s"$configPath.max-open-orders")
    val maxOrdersPerRequest = config.as[Int](s"$configPath.rest-order-limit")
    val maxOrdersPerAddress = config.as[Int](s"$configPath.max-orders-per-address")
    val baseAssets = config.as[List[String]](s"$configPath.price-assets")
    val basePairs: Seq[AssetPair] = config.getConfigList(s"$configPath.predefined-pairs").asScala.map { p: Config =>
      AssetPair.createAssetPair(p.as[String]("amountAsset"), p.as[String]("priceAsset")).get
    }
    val maxTimestampDiff = config.as[FiniteDuration](s"$configPath.max-timestamp-diff")

    val orderHistoryFile = config.getAs[File](s"$configPath.order-history-file")
    val txHistoryFile = config.getAs[File](s"$configPath.tx-history-file")

    val isMigrateToNewOrderHistoryStorage = !orderHistoryFile.exists(_.exists())

    val blacklistedAssets = config.as[List[String]](s"$configPath.blacklisted-assets")
    val blacklistedNames = config.as[List[String]](s"$configPath.blacklisted-names").map(_.r)

    val blacklistedAddresses = config.as[List[String]](s"$configPath.blacklisted-addresses")

    MatcherSettings(enabled, account, bindAddress, port, minOrderFee, orderMatchTxFee, journalDirectory,
      snapshotsDirectory, snapshotsInterval, orderCleanupInterval, orderHistoryCommitInterval, maxOpenOrders, baseAssets, basePairs, maxTimestampDiff,
      orderHistoryFile, isMigrateToNewOrderHistoryStorage, blacklistedAssets.toSet, blacklistedNames, txHistoryFile,
      maxOrdersPerRequest, blacklistedAddresses.toSet, maxOrdersPerAddress)
  }
}

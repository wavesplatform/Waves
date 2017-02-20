package com.wavesplatform.matcher

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._

import scala.concurrent.duration.FiniteDuration

case class MatcherSettings(enable: Boolean,
                           account: String,
                           bindAddress: String,
                           port: Int,
                           minOrderFee: Long,
                           orderMatchTxFee: Long,
                           journalDataDir: String,
                           snapshotsDataDir: String,
                           snapshotsInterval: FiniteDuration,
                           maxOpenOrders: Int
                          )


object MatcherSettings {
  val configPath: String = "waves.matcher"

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
    val maxOpenOrders = config.as[Int](s"$configPath.max-open-orders")

    MatcherSettings(enabled, account, bindAddress, port, minOrderFee, orderMatchTxFee, journalDirectory,
      snapshotsDirectory, snapshotsInterval, maxOpenOrders)
  }
}
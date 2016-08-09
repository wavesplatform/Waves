package com.wavesplatform.settings

import com.wavesplatform.{ChainParameters, MainNetParams, TestNetParams}
import scorex.settings.Settings
import scorex.transaction.TransactionSettings

import scala.util.Try

class WavesSettings(override val filename: String) extends Settings with TransactionSettings {
  lazy val loggingLevel = (settingsJSON \ "loggingLevel").asOpt[String].getOrElse("info").toLowerCase

  override lazy val genesisTimestamp = 1460678400000L
  override val MaxBlocksChunks = 20

  // TODO: Should be moved to Scorex TransactionSettings
  lazy val minimumTxFee = (settingsJSON \ "minimumTxFee").asOpt[Int].getOrElse(DefaultMinimumTxFee)
  private val DefaultMinimumTxFee = 100000

  val suspendedSenders = Try {
    (settingsJSON \ "suspendedSenders").as[List[String]]
  }.getOrElse(List[String]())

  lazy val isTestNet: Boolean = (settingsJSON \ "testnet").asOpt[Boolean].getOrElse(true)
  lazy val chainParams: ChainParameters = if (isTestNet) TestNetParams else MainNetParams
}

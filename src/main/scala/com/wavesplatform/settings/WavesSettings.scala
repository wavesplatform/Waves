package com.wavesplatform.settings

import com.typesafe.config.Config
import com.wavesplatform.matcher.MatcherSettings
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.EnumerationReader._

object LogLevel extends Enumeration {
  val TRACE = Value("TRACE")
  val DEBUG = Value("DEBUG")
  val INFO = Value("INFO")
  val WARN = Value("WARN")
  val ERROR = Value("ERROR")
}

case class WavesSettings(directory: String,
                         loggingLevel: LogLevel.Value,
                         networkSettings: NetworkSettings,
                         walletSettings: WalletSettings,
                         blockchainSettings: BlockchainSettings,
                         checkpointsSettings: CheckpointsSettings,
                         feesSettings: FeesSettings,
                         matcherSettings: MatcherSettings,
                         minerSettings: MinerSettings,
                         restAPISettings: RestAPISettings,
                         synchronizationSettings: SynchronizationSettings,
                         utxSettings: UTXSettings)

object WavesSettings {
  val configPath: String = "waves"
  def fromConfig(config: Config): WavesSettings = {
    val directory = config.as[String](s"$configPath.directory")
    val loggingLevel = config.as[LogLevel.Value](s"$configPath.logging-level")

    val networkSettings = NetworkSettings.fromConfig(config)
    val walletSettings = WalletSettings.fromConfig(config)
    val blockchainSettings = BlockchainSettings.fromConfig(config)
    val checkpointsSettings = CheckpointsSettings.fromConfig(config)
    val feesSettings = FeesSettings.fromConfig(config)
    val matcherSettings = MatcherSettings.fromConfig(config)
    val minerSettings = MinerSettings.fromConfig(config)
    val restAPISettings = RestAPISettings.fromConfig(config)
    val synchronizationSettings = SynchronizationSettings.fromConfig(config)
    val utxSettings = UTXSettings.fromConfig(config)

    WavesSettings(directory, loggingLevel, networkSettings, walletSettings, blockchainSettings, checkpointsSettings,
      feesSettings, matcherSettings, minerSettings, restAPISettings, synchronizationSettings, utxSettings)
  }
}

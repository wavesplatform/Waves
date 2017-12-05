package com.wavesplatform.settings

import com.typesafe.config.Config
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.metrics.Metrics
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.EnumerationReader._
import net.ceedubs.ficus.readers.URIReaders.javaURIReader

object LogLevel extends Enumeration {
  val TRACE = Value("TRACE")
  val DEBUG = Value("DEBUG")
  val INFO = Value("INFO")
  val WARN = Value("WARN")
  val ERROR = Value("ERROR")
}

case class WavesSettings(directory: String,
                         mvstorePageSplitSize: Int,
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
                         utxSettings: UtxSettings,
                         featuresSettings: FeaturesSettings,
                         metrics: Metrics.Settings)

object WavesSettings {

  import NetworkSettings.networkSettingsValueReader

  val configPath: String = "waves"

  def fromConfig(config: Config): WavesSettings = {
    val directory = config.as[String](s"$configPath.directory")
    val pageSplitSize = config.as[Int](s"$configPath.mvstore-page-split-size")
    val loggingLevel = config.as[LogLevel.Value](s"$configPath.logging-level")

    val networkSettings = config.as[NetworkSettings]("waves.network")
    val walletSettings = config.as[WalletSettings]("waves.wallet")
    val blockchainSettings = BlockchainSettings.fromConfig(config)
    val checkpointsSettings = CheckpointsSettings.fromConfig(config)
    val feesSettings = FeesSettings.fromConfig(config)
    val matcherSettings = MatcherSettings.fromConfig(config)
    val minerSettings = config.as[MinerSettings]("waves.miner")
    val restAPISettings = RestAPISettings.fromConfig(config)
    val synchronizationSettings = SynchronizationSettings.fromConfig(config)
    val utxSettings = config.as[UtxSettings]("waves.utx")
    val featuresSettings = config.as[FeaturesSettings]("waves.features")
    val metrics = config.as[Metrics.Settings]("metrics")

    WavesSettings(directory, pageSplitSize, loggingLevel, networkSettings, walletSettings, blockchainSettings, checkpointsSettings,
      feesSettings, matcherSettings, minerSettings, restAPISettings, synchronizationSettings, utxSettings,
      featuresSettings, metrics)
  }
}

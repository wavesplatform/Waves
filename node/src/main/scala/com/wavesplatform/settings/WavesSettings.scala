package com.wavesplatform.settings

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.metrics.Metrics
import scala.concurrent.duration.FiniteDuration
import pureconfig.*

case class WavesSettings(
    directory: String,
    ntpServer: String,
    maxTxErrorLogSize: Int,
    dbSettings: DBSettings,
    extensions: Seq[String],
    extensionsShutdownTimeout: FiniteDuration,
    networkSettings: NetworkSettings,
    walletSettings: WalletSettings,
    blockchainSettings: BlockchainSettings,
    minerSettings: MinerSettings,
    restAPISettings: RestAPISettings,
    synchronizationSettings: SynchronizationSettings,
    utxSettings: UtxSettings,
    featuresSettings: FeaturesSettings,
    rewardsSettings: RewardsVotingSettings,
    metrics: Metrics.Settings,
    enableLightMode: Boolean,
    config: Config
)

object WavesSettings {
  def fromRootConfig(rootConfig: Config): WavesSettings = {
    val waves             = rootConfig.getConfig("waves")
    val wavesConfigSource = ConfigSource.fromConfig(waves)

    val directory                 = wavesConfigSource.at("directory").loadOrThrow[String]
    val ntpServer                 = wavesConfigSource.at("ntp-server").loadOrThrow[String]
    val maxTxErrorLogSize         = wavesConfigSource.at("max-tx-error-log-size").loadOrThrow[Int]
    val dbSettings                = wavesConfigSource.at("db").loadOrThrow[DBSettings]
    val extensions                = wavesConfigSource.at("extensions").loadOrThrow[Seq[String]]
    val extensionsShutdownTimeout = wavesConfigSource.at("extensions-shutdown-timeout").loadOrThrow[FiniteDuration]
    val networkSettings           = wavesConfigSource.at("network").loadOrThrow[NetworkSettings]
    val walletSettings            = wavesConfigSource.at("wallet").loadOrThrow[WalletSettings]
    val blockchainSettings        = wavesConfigSource.at("blockchain").loadOrThrow[BlockchainSettings]
    val minerSettings             = wavesConfigSource.at("miner").loadOrThrow[MinerSettings]
    val restAPISettings           = wavesConfigSource.at("rest-api").loadOrThrow[RestAPISettings]
    val synchronizationSettings   = wavesConfigSource.at("synchronization").loadOrThrow[SynchronizationSettings]
    val utxSettings               = wavesConfigSource.at("utx").loadOrThrow[UtxSettings]
    val featuresSettings          = wavesConfigSource.at("features").loadOrThrow[FeaturesSettings]
    val rewardsSettings           = wavesConfigSource.at("rewards").loadOrThrow[RewardsVotingSettings]
    val metrics                   = ConfigSource.fromConfig(rootConfig).at("metrics").loadOrThrow[Metrics.Settings] // TODO: Move to waves section
    val enableLightMode           = wavesConfigSource.at("enable-light-mode").loadOrThrow[Boolean]

    WavesSettings(
      directory,
      ntpServer,
      maxTxErrorLogSize,
      dbSettings,
      extensions,
      extensionsShutdownTimeout,
      networkSettings,
      walletSettings,
      blockchainSettings,
      minerSettings,
      restAPISettings,
      synchronizationSettings,
      utxSettings,
      featuresSettings,
      rewardsSettings,
      metrics,
      enableLightMode,
      rootConfig
    )
  }

  def default(): WavesSettings = fromRootConfig(ConfigFactory.load())
}

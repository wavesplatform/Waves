package com.wavesplatform.settings

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.metrics.Metrics
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

import scala.concurrent.duration.FiniteDuration

case class WavesSettings(
    directory: String,
    ntpServer: String,
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
    config: Config
)

object WavesSettings extends CustomValueReaders {
  def fromRootConfig(rootConfig: Config): WavesSettings = {
    val waves = rootConfig.getConfig("waves")

    val directory                 = waves.as[String]("directory")
    val ntpServer                 = waves.as[String]("ntp-server")
    val dbSettings                = waves.as[DBSettings]("db")
    val extensions                = waves.as[Seq[String]]("extensions")
    val extensionsShutdownTimeout = waves.as[FiniteDuration]("extensions-shutdown-timeout")
    val networkSettings           = waves.as[NetworkSettings]("network")
    val walletSettings            = waves.as[WalletSettings]("wallet")
    val blockchainSettings        = waves.as[BlockchainSettings]("blockchain")
    val minerSettings             = waves.as[MinerSettings]("miner")
    val restAPISettings           = waves.as[RestAPISettings]("rest-api")
    val synchronizationSettings   = waves.as[SynchronizationSettings]("synchronization")
    val utxSettings               = waves.as[UtxSettings]("utx")
    val featuresSettings          = waves.as[FeaturesSettings]("features")
    val rewardsSettings           = waves.as[RewardsVotingSettings]("rewards")
    val metrics                   = rootConfig.as[Metrics.Settings]("metrics") // TODO: Move to waves section

    WavesSettings(
      directory,
      ntpServer,
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
      rootConfig
    )
  }

  def default(): WavesSettings = fromRootConfig(ConfigFactory.load())
}

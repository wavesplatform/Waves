package com.wavesplatform.settings

import com.typesafe.config.Config
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.metrics.Metrics
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

import scala.concurrent.duration._

case class WavesSettings(directory: String,
                         maxCacheSize: Int,
                         maxRollbackDepth: Int,
                         rememberBlocks: FiniteDuration,
                         ntpServer: String,
                         dbSettings: DBSettings,
                         networkSettings: NetworkSettings,
                         walletSettings: WalletSettings,
                         blockchainSettings: BlockchainSettings,
                         matcherSettings: MatcherSettings,
                         minerSettings: MinerSettings,
                         restAPISettings: RestAPISettings,
                         synchronizationSettings: SynchronizationSettings,
                         utxSettings: UtxSettings,
                         featuresSettings: FeaturesSettings,
                         metrics: Metrics.Settings)

object WavesSettings {

  import BlockchainSettings.blockChainSettingsValueReader
  import MatcherSettings.matcherSettingsValueReader
  import NetworkSettings.networkSettingsValueReader

  def fromRootConfig(rootConfig: Config): WavesSettings = {
    val waves = rootConfig.getConfig("waves")

    val directory               = waves.as[String]("directory")
    val maxCacheSize            = waves.as[Int]("max-cache-size")
    val maxRollbackDepth        = waves.as[Int]("max-rollback-depth")
    val rememberBlocks          = waves.as[FiniteDuration]("remember-blocks-interval-in-cache")
    val ntpServer               = waves.as[String]("ntp-server")
    val dbSettings              = waves.as[DBSettings]("db")
    val networkSettings         = waves.as[NetworkSettings]("network")
    val walletSettings          = waves.as[WalletSettings]("wallet")
    val blockchainSettings      = waves.as[BlockchainSettings]("blockchain")
    val matcherSettings         = waves.as[MatcherSettings]("matcher")
    val minerSettings           = waves.as[MinerSettings]("miner")
    val restAPISettings         = waves.as[RestAPISettings]("rest-api")
    val synchronizationSettings = waves.as[SynchronizationSettings]("synchronization")
    val utxSettings             = waves.as[UtxSettings]("utx")
    val featuresSettings        = waves.as[FeaturesSettings]("features")
    val metrics                 = rootConfig.as[Metrics.Settings]("metrics") // TODO: Move to waves section

    WavesSettings(
      directory,
      maxCacheSize,
      maxRollbackDepth,
      rememberBlocks,
      ntpServer,
      dbSettings,
      networkSettings,
      walletSettings,
      blockchainSettings,
      matcherSettings,
      minerSettings,
      restAPISettings,
      synchronizationSettings,
      utxSettings,
      featuresSettings,
      metrics
    )
  }
}

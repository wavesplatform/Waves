package com.wavesplatform.settings

import com.typesafe.config.Config
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.metrics.Metrics
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

import scala.concurrent.duration._

case class WavesSettings(directory: String,
                         dataDirectory: String,
                         maxCacheSize: Int,
                         maxRollbackDepth: Int,
                         rememberBlocks: FiniteDuration,
                         ntpServer: String,
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

  import NetworkSettings.networkSettingsValueReader

  val configPath: String = "waves"

  def fromConfig(config: Config): WavesSettings = {
    val directory               = config.as[String](s"$configPath.directory")
    val dataDirectory           = config.as[String](s"$configPath.data-directory")
    val maxCacheSize            = config.as[Int](s"$configPath.max-cache-size")
    val maxRollbackDepth        = config.as[Int](s"$configPath.max-rollback-depth")
    val rememberBlocks          = config.as[FiniteDuration](s"$configPath.remember-blocks-interval-in-cache")
    val ntpServer               = config.as[String](s"$configPath.ntp-server")
    val networkSettings         = config.as[NetworkSettings]("waves.network")
    val walletSettings          = config.as[WalletSettings]("waves.wallet")
    val blockchainSettings      = BlockchainSettings.fromConfig(config)
    val matcherSettings         = MatcherSettings.fromConfig(config)
    val minerSettings           = MinerSettings.fromConfig(config)
    val restAPISettings         = RestAPISettings.fromConfig(config)
    val synchronizationSettings = SynchronizationSettings.fromConfig(config)
    val utxSettings             = config.as[UtxSettings]("waves.utx")
    val featuresSettings        = config.as[FeaturesSettings]("waves.features")
    val metrics                 = config.as[Metrics.Settings]("metrics")

    WavesSettings(
      directory,
      dataDirectory,
      maxCacheSize,
      maxRollbackDepth,
      rememberBlocks,
      ntpServer,
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

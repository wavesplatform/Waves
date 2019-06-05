package com.wavesplatform.settings

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.metrics.Metrics
import kamon.Kamon
import kamon.influxdb.InfluxDBReporter
import kamon.system.SystemMetrics
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

import scala.concurrent.duration.FiniteDuration

case class WavesSettings(directory: String,
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
                         metrics: Metrics.Settings,
                         config: Config)

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
      metrics,
      rootConfig
    )
  }

  def loadRootConfig(external: Option[File] = None): WavesSettings = {
    val config = loadConfig(external.map(ConfigFactory.parseFile))
    val settings = fromRootConfig(config)

    // Initialize global var with actual address scheme
    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
    }

    if (config.getBoolean("kamon.enable")) {
      Kamon.addReporter(new InfluxDBReporter())
      SystemMetrics.startCollecting()
    }

    // DO NOT LOG BEFORE THIS LINE, THIS PROPERTY IS USED IN logback.xml
    System.setProperty("waves.directory", config.getString("waves.directory"))

    // IMPORTANT: to make use of default settings for histograms and timers, it's crucial to reconfigure Kamon with
    //            our merged config BEFORE initializing any metrics, including in settings-related companion objects
    Kamon.reconfigure(config)

    settings
  }
}

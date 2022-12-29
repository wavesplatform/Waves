package com.wavesplatform.ride.app

import com.typesafe.config.{Config, ConfigFactory, ConfigParseOptions}
import com.wavesplatform.Version
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.settings.*
import com.wavesplatform.utils.{Misconfiguration, ScorexLogging, forceStopApplication}

import java.io.File

object AppInitializer extends ScorexLogging {
  def init(externalConfig: Option[File] = None): (Config, RideRunnerGlobalSettings) = {
    val maybeExternalConfig =
      try externalConfig.map(f => ConfigFactory.parseFile(f.getAbsoluteFile, ConfigParseOptions.defaults().setAllowMissing(false)))
      catch {
        case e: Throwable =>
          log.error(s"Couldn't read ${externalConfig.get.toPath.toAbsolutePath}", e)
          forceStopApplication(Misconfiguration)
          None
      }

    val config = loadConfig(maybeExternalConfig)

    // DO NOT LOG BEFORE THIS LINE, THIS PROPERTY IS USED IN logback.xml
    System.setProperty("waves.directory", config.getString("waves.directory"))
    if (config.hasPath("waves.config.directory")) System.setProperty("waves.config.directory", config.getString("waves.config.directory"))

    val settings = RideRunnerGlobalSettings.fromRootConfig(config)
    val network  = settings.blockchain.addressSchemeCharacter
    log.info(s"Starting ${Version.VersionString}...")
    log.info(s"Chosen network: $network / ${network.toByte}")

    // Initialize global var with actual address scheme
    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = network.toByte
    }

    (config, settings)
  }
}

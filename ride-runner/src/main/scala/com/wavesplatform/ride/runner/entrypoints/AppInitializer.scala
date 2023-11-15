package com.wavesplatform.ride.runner.entrypoints

import com.google.common.io.{MoreFiles, RecursiveDeleteOption}
import com.typesafe.config.{Config, ConfigFactory, ConfigParseOptions}
import com.wavesplatform.Version
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.ride.runner.entrypoints.settings.RideRunnerGlobalSettings
import com.wavesplatform.settings.{loadConfig as _, *}
import com.wavesplatform.utils.{Misconfiguration, ScorexLogging, forceStopApplication}

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

object AppInitializer extends ScorexLogging {
  def init(checkDb: Boolean = true, externalConfig: Option[File] = None): (Config, RideRunnerGlobalSettings) = {
    val config = loadConfig(externalConfig)

    // DO NOT LOG BEFORE THIS LINE, THIS PROPERTY IS USED IN logback.xml
    System.setProperty("waves.directory", config.getString("waves.directory"))
    if (config.hasPath("waves.config.directory")) System.setProperty("waves.config.directory", config.getString("waves.config.directory"))

    // Can't use config.getString, because a part of config is hard-coded in BlockchainSettings
    val blockchainSettings = BlockchainSettings.fromRootConfig(config)
    setupChain(blockchainSettings.addressSchemeCharacter)

    val settings = RideRunnerGlobalSettings.fromRootConfig(config)
    log.info(s"Starting ${Version.VersionString}...")

    if (checkDb) checkDbVersion(settings)
    (config, settings)
  }

  def loadConfig(externalConfig: Option[File]): Config = {
    val maybeExternalConfig =
      try externalConfig.map(f => ConfigFactory.parseFile(f.getAbsoluteFile, ConfigParseOptions.defaults().setAllowMissing(false)))
      catch {
        case e: Throwable =>
          log.error(s"Couldn't read ${externalConfig.get.toPath.toAbsolutePath}", e)
          forceStopApplication(Misconfiguration)
          None
      }

    com.wavesplatform.settings.loadConfig(maybeExternalConfig)
  }

  def setupChain(network: Char): Unit = if (AddressScheme.current.chainId != network.toByte) {
    log.info(s"Chosen network: $network / ${network.toByte}")

    // Initialize global var with actual address scheme
    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = network.toByte
    }
  }

  private def checkDbVersion(settings: RideRunnerGlobalSettings): Unit = {
    val rootPath = Paths.get(settings.rideRunner.db.directory, "..").normalize()
    Files.createDirectories(rootPath)

    val versionFilePath = rootPath.resolve("version")
    val (cleanup, updateVersion) =
      if (versionFilePath.toFile.exists()) {
        val rawVersion = new String(Files.readAllBytes(versionFilePath), StandardCharsets.UTF_8).trim
        rawVersion.toIntOption match {
          case Some(version) =>
            if (version != settings.rideRunner.db.version) {
              log.info(s"Database version changed from $version to ${settings.rideRunner.db.version}, removing...")
              (true, true)
            } else {
              log.info(s"Current database version: $version")
              (false, false)
            }
          case None =>
            log.error(
              s"Database version file $versionFilePath contains invalid content! " +
                s"Please contact with an administrator, content: ${rawVersion.take(100)}..."
            )
            (true, true)
        }
      } else {
        log.info("Database version file doesn't exist, creating...")
        (false, true)
      }

    if (cleanup) {
      val logPath = rootPath.resolve("log.txt").toAbsolutePath.toString
      rootPath.toFile.listFiles().foreach { file =>
        if (file.getAbsolutePath != versionFilePath.toAbsolutePath.toString && file.getAbsolutePath != logPath)
          MoreFiles.deleteRecursively(file.toPath, RecursiveDeleteOption.ALLOW_INSECURE)
      }
    }

    if (updateVersion) Files.write(versionFilePath, settings.rideRunner.db.version.toString.getBytes(StandardCharsets.UTF_8))
  }
}

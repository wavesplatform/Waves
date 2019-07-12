package com.wavesplatform.db
import java.nio.file.Files

import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.WavesSettings

trait WithDBSettings {
  lazy val dbSettings = WavesSettings.fromRootConfig(ConfigFactory.load()).dbSettings.copy(directory = Files.createTempDirectory("lvl").toString)
  lazy val maxCacheSize: Int = dbSettings.maxCacheSize
}

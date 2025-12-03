package com.wavesplatform.db
import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.WavesSettings

trait DBCacheSettings {
  lazy val dbSettings        = WavesSettings.fromRootConfig(ConfigFactory.load()).dbSettings
  lazy val maxCacheSize: Int = dbSettings.maxCacheSize
}

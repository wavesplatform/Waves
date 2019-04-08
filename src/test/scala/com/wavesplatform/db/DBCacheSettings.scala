package com.wavesplatform.db
import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.WavesSettings

trait DBCacheSettings {
  lazy val maxCacheSize: Int = {
    val settings = WavesSettings.fromRootConfig(ConfigFactory.load())
    settings.dbSettings.maxCacheSize
  }
}

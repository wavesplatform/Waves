package com.wavesplatform.http

import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.RestAPISettings
import scorex.crypto.encode.Base58
import scorex.crypto.hash.SecureCryptographicHash

trait RestAPISettingsHelper {
  def apiKey: String = "test_api_key"
  lazy val restAPISettings = {
    val keyHash = Base58.encode(SecureCryptographicHash(apiKey))
    RestAPISettings.fromConfig(
      ConfigFactory
        .parseString(s"waves.rest-api.api-key-hash = $keyHash")
        .withFallback(ConfigFactory.load()))
  }
}

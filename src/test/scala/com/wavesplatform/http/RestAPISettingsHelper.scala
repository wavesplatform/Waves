package com.wavesplatform.http

import com.typesafe.config.ConfigFactory
import com.wavesplatform.crypto
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.utils.Base58

trait RestAPISettingsHelper {
  def apiKey: String = "test_api_key"

  lazy val restAPISettings = {
    val keyHash = Base58.encode(crypto.secureHash(apiKey.getBytes()))
    RestAPISettings.fromConfig(
      ConfigFactory
        .parseString(s"waves.rest-api.api-key-hash = $keyHash")
        .withFallback(ConfigFactory.load()))
  }
}

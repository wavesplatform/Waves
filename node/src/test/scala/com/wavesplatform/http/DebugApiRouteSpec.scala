package com.wavesplatform.http

import com.wavesplatform.api.http.ApiError.ApiKeyNotValid
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.{NTPTime, TestWallet}

//noinspection ScalaStyle
class DebugApiRouteSpec extends RouteSpec("/debug") with RestAPISettingsHelper with TestWallet with NTPTime {

  private val sampleConfig  = com.typesafe.config.ConfigFactory.load()
  private val wavesSettings = WavesSettings.fromRootConfig(sampleConfig)
  private val configObject  = sampleConfig.root()
  private val route =
    DebugApiRoute(wavesSettings, ntpTime, null, null, null, null, null, null, null, null, null, null, null, null, null, configObject).route

  routePath("/configInfo") - {
    "requires api-key header" in {
      Get(routePath("/configInfo?full=true")) ~> route should produce(ApiKeyNotValid)
      Get(routePath("/configInfo?full=false")) ~> route should produce(ApiKeyNotValid)
    }
  }
}

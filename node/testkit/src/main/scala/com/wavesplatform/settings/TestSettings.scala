package com.wavesplatform.settings

import com.typesafe.config.ConfigFactory
import com.wavesplatform.features.BlockchainFeatures

object TestSettings {
  val Default: WavesSettings = WavesSettings.fromRootConfig(ConfigFactory.load())

  implicit class WavesSettingsExt(val ws: WavesSettings) extends AnyVal {
    def withFunctionalitySettings(fs: FunctionalitySettings): WavesSettings =
      ws.copy(blockchainSettings = ws.blockchainSettings.copy(functionalitySettings = fs))

    def withNG: WavesSettings =
      ws.withFunctionalitySettings(
        ws.blockchainSettings.functionalitySettings.copy(
          blockVersion3AfterHeight = 0,
          preActivatedFeatures = ws.blockchainSettings.functionalitySettings.preActivatedFeatures ++ Map(BlockchainFeatures.NG.id -> 0)
        )
      )
  }
}

package com.wavesplatform.settings

import com.typesafe.config.ConfigFactory
import com.wavesplatform.test.FlatSpec
import pureconfig.ConfigSource

class FeaturesSettingsSpecification extends FlatSpec {
  "FeaturesSettings" should "read values" in {
    val config = ConfigFactory
      .parseString("""
                     |waves {
                     |  features {
                     |    auto-shutdown-on-unsupported-feature = yes
                     |    supported = [123,124,135]
                     |  }
                     |}
      """.stripMargin)
      .resolve()

    val settings = ConfigSource.fromConfig(config).at("waves.features").loadOrThrow[FeaturesSettings]

    settings.autoShutdownOnUnsupportedFeature should be(true)
    settings.supported shouldEqual List(123, 124, 135)
  }
}

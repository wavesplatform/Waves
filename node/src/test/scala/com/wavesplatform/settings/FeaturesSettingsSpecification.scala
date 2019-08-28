package com.wavesplatform.settings

import com.typesafe.config.ConfigFactory
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import org.scalatest.{FlatSpec, Matchers}

class FeaturesSettingsSpecification extends FlatSpec with Matchers {
  "FeaturesSettings" should "read values" in {
    val config = ConfigFactory.parseString("""
        |waves {
        |  features {
        |    auto-shutdown-on-unsupported-feature = yes
        |    supported = [123,124,135]
        |  }
        |}
      """.stripMargin).resolve()

    val settings = config.as[FeaturesSettings]("waves.features")

    settings.autoShutdownOnUnsupportedFeature should be(true)
    settings.supported shouldEqual List(123, 124, 135)
  }
}

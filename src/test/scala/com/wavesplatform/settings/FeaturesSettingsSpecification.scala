package com.wavesplatform.settings

import java.time.Duration

import com.typesafe.config.ConfigFactory
import org.scalatest.{FlatSpec, Matchers}

class FeaturesSettingsSpecification extends FlatSpec with Matchers {
  "FeaturesSettings" should "read values" in {
    val config = ConfigFactory.parseString(
      """
        |waves {
        |  features {
        |    auto-activate: yes
        |    auto-stop: yes
        |    supported: [123,124,135]
        |  }
        |}
      """.stripMargin).resolve()

    val settings = FeaturesSettings.fromConfig(config)

    settings.autoActivate should be(true)
    settings.autoStop should be(true)
    settings.supported shouldEqual List(123,124,135)
  }
}

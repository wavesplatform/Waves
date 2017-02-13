package com.wavesplatform.matcher.settings

import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.CheckpointsSettings
import org.scalatest.{FlatSpec, Matchers}

class CheckpointsSettingsSpecification extends FlatSpec with Matchers {
  "CheckpointsSettings" should "read values" in {
    val config = ConfigFactory.parseString(
      """
        |waves {
        |  checkpoints {
        |    public-key: "BASE58PUBLICKEY"
        |  }
        |}
      """.stripMargin).resolve()
    val settings = CheckpointsSettings.fromConfig(config)

    settings.publicKey should be ("BASE58PUBLICKEY")
  }
}

package com.wavesplatform.settings

import com.typesafe.config.ConfigFactory
import com.wavesplatform.state2.ByteStr
import org.scalatest.{FlatSpec, Matchers}
import scorex.crypto.encode.Base58

class CheckpointsSettingsSpecification extends FlatSpec with Matchers {
  "CheckpointsSettings" should "read values" in {
    val config = ConfigFactory.parseString(
      """
        |waves {
        |  checkpoints {
        |    public-key: "BASE58PUBKEY"
        |  }
        |}
      """.stripMargin).resolve()
    val settings = CheckpointsSettings.fromConfig(config)

    settings.publicKey should be (ByteStr(Base58.decode("BASE58PUBKEY").get))
  }
}

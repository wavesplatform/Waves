package com.wavesplatform.settings

import com.typesafe.config.ConfigFactory
import com.wavesplatform.common.state.ByteStr
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import org.scalatest.{FlatSpec, Matchers}

class WalletSettingsSpecification extends FlatSpec with Matchers {
  "WalletSettings" should "read values from config" in {
    val config   = loadConfig(ConfigFactory.parseString("""waves.wallet {
        |  password: "some string as password"
        |  seed: "BASE58SEED"
        |}""".stripMargin))
    val settings = config.as[WalletSettings]("waves.wallet")

    settings.seed should be(Some(ByteStr.decodeBase58("BASE58SEED").get))
    settings.password should be(Some("some string as password"))
  }
}

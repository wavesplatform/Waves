package com.wavesplatform.matcher.settings

import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.WalletSettings
import org.scalatest.{FlatSpec, Matchers}

class WalletSettingsSpecification extends FlatSpec with Matchers {
  "WalletSettings" should "read values from config" in {
    val config = ConfigFactory.parseString(
      """
        |waves {
        |  directory: "/waves"
        |  wallet {
        |    file: ${waves.directory}"/wallet/wallet.dat"
        |    password: "some string as password"
        |    seed: "BASE58SEED"
        |  }
        |}
      """.stripMargin).resolve()
    val settings = WalletSettings.fromConfig(config)

    settings.seed should be("BASE58SEED")
    settings.file should be("/waves/wallet/wallet.dat")
    settings.password should be ("some string as password")
  }
}

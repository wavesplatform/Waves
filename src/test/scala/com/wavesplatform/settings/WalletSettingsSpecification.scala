package com.wavesplatform.settings

import com.typesafe.config.ConfigFactory
import com.wavesplatform.state2.ByteStr
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import org.scalatest.{FlatSpec, Matchers}

class WalletSettingsSpecification extends FlatSpec with Matchers {
  "WalletSettings" should "read values from config" in {
    val config = loadConfig(ConfigFactory.parseString(
      """waves.wallet {
        |  password: "some string as password"
        |  seed: "BASE58SEED"
        |}""".stripMargin))
    val settings = config.as[WalletSettings]("waves.wallet")

    settings.seed should be(Some(ByteStr.decodeBase58("BASE58SEED").get))
    settings.password should be("some string as password")
  }

  "WalletSettings" should "create correct settings from config" in {
    val config = loadConfig(ConfigFactory.parseString(
      """waves {
        |  directory: ${user.home}"/waves"
        |  wallet {
        |    password: "some string as password"
        |    seed: "BASE58SEED"
        |  }
        |}""".stripMargin
    ))

    val walletSettings = WalletSettings.fromConfig(config)

    walletSettings.file.isDefined shouldEqual true
    walletSettings.file.get.getAbsolutePath shouldEqual s"${config.getString("waves.directory")}/wallet/wallet.dat"
    walletSettings.seed shouldEqual Some(ByteStr.decodeBase58("BASE58SEED").get)
    walletSettings.password shouldEqual "some string as password"
  }

  "WalletSettings" should "create default wallet file" in {
    val config = loadConfig(ConfigFactory.parseString(
      """waves {
        |  wallet {
        |    file: ${waves.directory}"/wallet/wallet.dat"
        |    password: "some string as password"
        |    seed: "BASE58SEED"
        |  }
        |}""".stripMargin
    ))

    val walletSettings = WalletSettings.fromConfig(config)

    walletSettings.file.isDefined shouldEqual true
    walletSettings.file.get.getAbsolutePath shouldEqual s"${config.getString("waves.directory")}/wallet/wallet.dat"
    walletSettings.seed shouldEqual Some(ByteStr.decodeBase58("BASE58SEED").get)
    walletSettings.password shouldEqual "some string as password"
  }
}

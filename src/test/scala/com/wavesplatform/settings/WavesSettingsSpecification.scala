package com.wavesplatform.settings

import java.io.File

import com.typesafe.config.ConfigFactory
import org.scalatest.{FlatSpec, Matchers}

class WavesSettingsSpecification extends FlatSpec with Matchers {

  val expectedDir =
    ConfigFactory
      .defaultOverrides()
      .withFallback(ConfigFactory.parseString(s"waves.directory=${com.wavesplatform.settings.defaultDirectory}"))
      .resolve()
      .getString("waves.directory")

  private def config(configName: String) = {
    WavesSettings.fromConfig(
      com.wavesplatform.settings.loadConfig(
        ConfigFactory.parseFile(new File(s"waves-$configName.conf"))
      )
    )
  }

  def testConfig(configName: String)(additionalChecks: WavesSettings => Unit = _ => ()) {
    "WavesSettings" should s"read values from default config with $configName overrides" in {
      val settings = config(configName)

      settings.directory should be(expectedDir)
      settings.networkSettings should not be null
      settings.walletSettings should not be null
      settings.blockchainSettings should not be null
      settings.matcherSettings should not be null
      settings.minerSettings should not be null
      settings.restAPISettings should not be null
      settings.synchronizationSettings should not be null
      settings.utxSettings should not be null
      additionalChecks(settings)
    }
  }

  testConfig("mainnet")()
  testConfig("testnet")()
  testConfig("devnet")()

  "WavesSettings" should "resolve folders correctly" in {
    val config = loadConfig(ConfigFactory.parseString(s"""waves {
         |  directory = "/xxx"
         |  data-directory = "/xxx/data"
         |  ntp-server = "example.com"
         |}""".stripMargin))

    val settings = WavesSettings.fromConfig(config.resolve())

    settings.directory should be("/xxx")
    settings.dataDirectory should be("/xxx/data")
    settings.ntpServer should be("example.com")
    settings.networkSettings.file should be(Some(new File("/xxx/peers.dat")))
    settings.walletSettings.file should be(Some(new File("/xxx/wallet/wallet.dat")))
    settings.matcherSettings.journalDataDir should be("/xxx/matcher/journal")
    settings.matcherSettings.snapshotsDataDir should be("/xxx/matcher/snapshots")
  }

}

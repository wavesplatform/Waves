package com.wavesplatform.settings

import java.io.File

import com.typesafe.config.ConfigFactory
import org.scalatest.{FlatSpec, Matchers}

class WavesSettingsSpecification extends FlatSpec with Matchers {
  private val home = System.getProperty("user.home")

  private def config(configName: String) =
    WavesSettings.fromConfig(ConfigFactory.parseFile(new File(s"waves-$configName.conf")).withFallback(ConfigFactory.load()))

  def testConfig(configName: String)(additionalChecks: WavesSettings => Unit = _ => ()) {
    "WavesSettings" should s"read values from default config with $configName overrides" in {
      val settings = config(configName)

      settings.directory should be(home + "/waves")
      settings.networkSettings should not be null
      settings.walletSettings should not be null
      settings.blockchainSettings should not be null
      settings.checkpointsSettings should not be null
      settings.feesSettings should not be null
      settings.matcherSettings should not be null
      settings.minerSettings should not be null
      settings.restAPISettings should not be null
      settings.synchronizationSettings should not be null
      settings.utxSettings should not be null
      additionalChecks(settings)
    }
  }

  testConfig("mainnet") {
    _.loggingLevel should be(LogLevel.INFO)
  }

  testConfig("testnet")()
  testConfig("devnet")()

  "WavesSettings" should "resolve folders correctly" in {
    val config = loadConfig(ConfigFactory.parseString(
      """waves {
        |  logging-level = TRACE
        |  directory = "/xxx"
        |}""".stripMargin))

    val settings = WavesSettings.fromConfig(config.resolve())

    settings.directory should be("/xxx")
    settings.networkSettings.file should be(Some(new File("/xxx/data/peers.dat")))
    settings.walletSettings.file should be(Some(new File("/xxx/wallet/wallet.dat")))
    settings.loggingLevel should be(LogLevel.TRACE)
    settings.blockchainSettings.blockchainFile should be(Some(new File("/xxx/data/blockchain.dat")))
    settings.blockchainSettings.stateFile should be(Some(new File("/xxx/data/state.dat")))
    settings.blockchainSettings.checkpointFile should be(Some(new File("/xxx/data/checkpoint.dat")))
    settings.matcherSettings.journalDataDir should be ("/xxx/matcher/journal")
    settings.matcherSettings.snapshotsDataDir should be ("/xxx/matcher/snapshots")
  }

}

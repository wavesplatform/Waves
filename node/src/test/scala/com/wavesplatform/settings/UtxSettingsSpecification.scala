package com.wavesplatform.settings

import com.typesafe.config.ConfigFactory
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import org.scalatest.{FlatSpec, Matchers}

class UtxSettingsSpecification extends FlatSpec with Matchers {
  "UTXSettings" should "read values" in {
    val config = ConfigFactory.parseString("""waves {
        |  utx {
        |    max-size = 100
        |    max-bytes-size = 100
        |    max-scripted-size = 100
        |    blacklist-sender-addresses = ["a"]
        |    allow-blacklisted-transfer-to = ["b"]
        |    allow-transactions-from-smart-accounts = false
        |    allow-skip-checks = false
        |  }
        |}""".stripMargin).resolve()

    val settings = config.as[UtxSettings]("waves.utx")
    settings.maxSize shouldBe 100
    settings.maxBytesSize shouldBe 100L
    settings.maxScriptedSize shouldBe 100
    settings.blacklistSenderAddresses shouldBe Set("a")
    settings.allowBlacklistedTransferTo shouldBe Set("b")
    settings.allowTransactionsFromSmartAccounts shouldBe false
    settings.allowSkipChecks shouldBe false
  }
}

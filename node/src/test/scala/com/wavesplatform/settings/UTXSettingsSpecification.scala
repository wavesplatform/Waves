package com.wavesplatform.settings

import com.typesafe.config.ConfigFactory
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import org.scalatest.{FlatSpec, Matchers}

class UTXSettingsSpecification extends FlatSpec with Matchers {
  "UTXSettings" should "read values" in {
    val config = ConfigFactory.parseString("""waves {
        |  utx {
        |    max-size = 100
        |    max-bytes-size = 100
        |    max-scripted-size = 100
        |    blacklist-sender-addresses = ["a"]
        |    allow-rebroadcasting = true
        |    allow-blacklisted-transfer-to = ["b"]
        |    allow-transactions-from-smart-accounts = false
        |    allow-skip-checks = false
        |    max-pack-time = 5s
        |  }
        |}""".stripMargin).resolve()

    val settings = config.as[UtxSettings]("waves.utx")
    settings.maxSize shouldBe 100
    settings.maxBytesSize shouldBe 100L
    settings.maxScriptedSize shouldBe 100
    settings.blacklistSenderAddresses shouldBe Set("a")
    settings.allowRebroadcasting shouldBe true
    settings.allowBlacklistedTransferTo shouldBe Set("b")
    settings.allowTransactionsFromSmartAccounts shouldBe false
    settings.allowSkipChecks shouldBe false
  }
}

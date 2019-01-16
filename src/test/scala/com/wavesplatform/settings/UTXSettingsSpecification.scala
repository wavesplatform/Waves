package com.wavesplatform.settings

import com.typesafe.config.ConfigFactory
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class UTXSettingsSpecification extends FlatSpec with Matchers {
  "UTXSettings" should "read values" in {
    val config = ConfigFactory.parseString("""waves {
        |  utx {
        |    max-size = 100
        |    max-bytes-size = 100
        |    cleanup-interval = 10m
        |    blacklist-sender-addresses = ["a"]
        |    allow-blacklisted-transfer-to = ["b"]
        |    allow-transactions-from-smart-accounts = false
        |  }
        |}""".stripMargin).resolve()

    val settings = config.as[UtxSettings]("waves.utx")
    settings.maxSize shouldBe 100
    settings.maxBytesSize shouldBe 100L
    settings.cleanupInterval shouldBe 10.minutes
    settings.blacklistSenderAddresses shouldBe Set("a")
    settings.allowBlacklistedTransferTo shouldBe Set("b")
    settings.allowTransactionsFromSmartAccounts shouldBe false
  }
}

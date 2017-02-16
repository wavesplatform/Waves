package com.wavesplatform.matcher.settings

import com.typesafe.config.ConfigFactory
import com.wavesplatform.matcher.MatcherSettings
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class MatcherSettingsSpecification extends FlatSpec with Matchers {
  "MatcherSettings" should "read values" in {
    val config = ConfigFactory.parseString(
      """
        |waves {
        |  directory: "/waves"
        |  matcher {
        |    enable: yes
        |    account: "BASE58MATCHERACCOUNT"
        |    bind-address: "127.0.0.1"
        |    port: 6886
        |    min-order-fee: 100000
        |    order-match-tx-fee: 100000
        |    journal-directory: ${waves.directory}"/journal"
        |    snapshots-directory: ${waves.directory}"/snapshots"
        |    snapshots-interval: 1d
        |    max-open-orders: 1000
        |  }
        |}
      """.stripMargin).resolve()

    val settings = MatcherSettings.fromConfig(config)
    settings.enable should be(true)
    settings.account should be("BASE58MATCHERACCOUNT")
    settings.bindAddress should be("127.0.0.1")
    settings.port should be(6886)
    settings.minOrderFee should be(100000)
    settings.orderMatchTxFee should be(100000)
    settings.journalDataDir should be("/waves/journal")
    settings.snapshotsDataDir should be("/waves/snapshots")
    settings.snapshotsInterval should be(1.day)
    settings.maxOpenOrders should be(1000)
  }
}

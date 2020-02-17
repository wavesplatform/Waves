package com.wavesplatform.settings

import com.typesafe.config.ConfigFactory
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class MinerSettingsSpecification extends FlatSpec with Matchers {
  "MinerSettings" should "read values" in {
    val config = ConfigFactory.parseString("""
        |waves {
        |  miner {
        |    enable: yes
        |    quorum: 1
        |    interval-after-last-block-then-generation-is-allowed: 1d
        |    no-quorum-mining-delay = 5s
        |    micro-block-interval: 5s
        |    minimal-block-generation-offset: 500ms
        |    max-transactions-in-micro-block: 400
        |    min-micro-block-age: 3s
        |    max-pack-time: 5s
        |  }
        |}
      """.stripMargin).resolve()

    val settings = config.as[MinerSettings]("waves.miner")

    settings.enable should be(true)
    settings.quorum should be(1)
    settings.microBlockInterval should be(5.seconds)
    settings.noQuorumMiningDelay should be(5.seconds)
    settings.minimalBlockGenerationOffset should be(500.millis)
    settings.maxTransactionsInMicroBlock should be(400)
    settings.minMicroBlockAge should be(3.seconds)
  }
}

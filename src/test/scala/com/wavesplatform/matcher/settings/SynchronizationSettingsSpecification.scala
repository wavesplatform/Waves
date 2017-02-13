package com.wavesplatform.matcher.settings

import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.SynchronizationSettings
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class SynchronizationSettingsSpecification extends FlatSpec with Matchers {
  "SynchronizationSettings" should "read values" in {
    val config = ConfigFactory.parseString(
      """
        |waves {
        |  synchronization {
        |    max-rollback: 100
        |    max-chain-length: 101
        |    load-entire-chain: yes
        |    synchronization-timeout: 30s
        |    pin-to-initial-peer: yes
        |    retries-before-blacklisting: 2
        |    operation-retires: 3
        |    score-broadcast-interval: 30s
        |    score-ttl: 90s
        |  }
        |}
      """.stripMargin).resolve()

    val settings = SynchronizationSettings.fromConfig(config)
    settings.maxRollback should be(100)
    settings.maxChainLength should be(101)
    settings.loadEntireChain should be(true)
    settings.synchronizationTimeout should be(30.seconds)
    settings.pinToInitialPeer should be(true)
    settings.retriesBeforeBlacklisting should be(2)
    settings.operationRetries should be(3)
    settings.scoreBroadcastInterval should be(30.seconds)
    settings.scoreTTL should be(90.seconds)
  }
}

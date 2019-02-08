package com.wavesplatform.settings

import com.typesafe.config.ConfigFactory
import com.wavesplatform.network.InvalidBlockStorageImpl.InvalidBlockStorageSettings
import com.wavesplatform.settings.SynchronizationSettings.{HistoryReplierSettings, MicroblockSynchronizerSettings, UtxSynchronizerSettings}
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class SynchronizationSettingsSpecification extends FlatSpec with Matchers {
  "SynchronizationSettings" should "read values" in {
    val config = ConfigFactory.parseString("""
        |waves {
        |  synchronization {
        |    max-rollback = 100
        |    max-chain-length = 101
        |    synchronization-timeout = 30s
        |    score-ttl = 90s
        |
        |    invalid-blocks-storage {
        |      max-size = 40000
        |      timeout = 2d
        |    }
        |
        |    history-replier {
        |      max-micro-block-cache-size = 5
        |      max-block-cache-size = 2
        |    }
        |
        |    utx-synchronizer {
        |      network-tx-cache-size = 7000000
        |      network-tx-cache-time = 70s
        |      max-buffer-size = 777
        |      max-buffer-time = 999ms
        |      max-queue-size = 7777
        |      parallelism = 4
        |      max-threads = 2
        |    }
        |
        |    micro-block-synchronizer {
        |      wait-response-timeout: 5s
        |      processed-micro-blocks-cache-timeout: 2s
        |      inv-cache-timeout: 3s
        |    }
        |  }
        |}
      """.stripMargin).resolve()

    val settings = SynchronizationSettings.fromConfig(config)
    settings.maxRollback should be(100)
    settings.maxChainLength should be(101)
    settings.synchronizationTimeout should be(30.seconds)
    settings.scoreTTL should be(90.seconds)
    settings.invalidBlocksStorage shouldBe InvalidBlockStorageSettings(
      maxSize = 40000,
      timeout = 2.days
    )
    settings.microBlockSynchronizer shouldBe MicroblockSynchronizerSettings(
      waitResponseTimeout = 5.seconds,
      processedMicroBlocksCacheTimeout = 2.seconds,
      invCacheTimeout = 3.seconds
    )
    settings.historyReplierSettings shouldBe HistoryReplierSettings(
      maxMicroBlockCacheSize = 5,
      maxBlockCacheSize = 2
    )

    settings.utxSynchronizerSettings shouldBe UtxSynchronizerSettings(7000000, 70.seconds, 777, 999.millis, 4, 2, 7777)
  }
}

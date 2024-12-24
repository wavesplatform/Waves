package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.{BaseFunSuite, NodeConfigs}
import com.wavesplatform.transaction.TxHelpers

import scala.concurrent.duration.*

class ContinuousMiningTestSuite extends BaseFunSuite {
  // private val txnSendingInterval = 4.millis

  override protected val nodeConfigs: Seq[Config] = Seq(
    ConfigFactory
      .parseString(s"""waves {
                      |  blockchain.custom.functionality.pre-activated-features = {
                      |    1  = 0
                      |    16 = 0
                      |    17 = 0
                      |    18 = 0
                      |    19 = 0
                      |    20 = 0
                      |    21 = 0
                      |    22 = 0
                      |    23 = 0
                      |  }
                      |  miner {
                      |    quorum = 0
                      |    micro-block-interval = 6ms
                      |    min-micro-block-age = 10ms # 100ms
                      |  }
                      |}""".stripMargin)
      .withFallback(NodeConfigs.Miners.head)
      .resolve()
  )

  test("Can continue mining with a last micro block removing") {
    val waitHeight = miner.height + 10
    var i = 0
    while (miner.height < waitHeight) {
      if (i == 0) miner.transfer(miner.keyPair, TxHelpers.secondAddress.toString, 1, minFee)
      else i = (i + 1) % 5
      // Thread.sleep(txnSendingInterval.toMillis)
    }
  }
}


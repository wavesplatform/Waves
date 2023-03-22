package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.{KeyPair, PrivateKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.test.*
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.BaseFunSuite
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.sync.SeveralAccountMiningSuite.*

import scala.concurrent.duration.*

class SeveralAccountMiningSuite extends BaseFunSuite {

  override def nodeConfigs: Seq[Config] = Configs

  test("only private keys from config used for mining when specified") {
    val minerBalance1 = miner.balance(MinerPk1.toAddress.toString).balance
    val fromHeight    = miner.height
    miner.waitForHeight(miner.height + 5, 2.minutes)
    val tx                  = miner.transfer(MinerPk1, notMiner.address, minerBalance1 - 10.waves, waitForTx = true)
    val minerTransferHeight = nodes.waitForTransaction(tx.id).height
    nodes.waitForHeight(minerTransferHeight + 5)
    val pkMiners = Set(MinerPk1.toAddress.toString, MinerPk2.toAddress.toString)
    notMiner.blockSeq(fromHeight, notMiner.height).foreach { block =>
      if (block.height <= minerTransferHeight) {
        pkMiners should contain(block.generator)
      } else {
        block.generator shouldBe MinerPk2.toAddress.toString
      }
    }
  }
}

object SeveralAccountMiningSuite {
  val MinerPk1: KeyPair = getNodeKeyPair(2)
  val MinerPk2: KeyPair = getNodeKeyPair(3)

  private val minerConfig =
    ConfigFactory.parseString(s"""
                                 |waves {
                                 |  blockchain.custom.genesis {
                                 |     average-block-delay = 3s
                                 |  }
                                 |  blockchain.custom.functionality {
                                 |    pre-activated-features.1 = 0
                                 |    min-block-time = 3s
                                 |  }
                                 |  miner {
                                 |    quorum = 0
                                 |    private-keys = ["${MinerPk1.privateKey.toString}", "${MinerPk2.privateKey.toString}"]
                                 |  }
                                 |}""".stripMargin)

  private val nonMinerConfig =
    ConfigFactory.parseString(s"""
                                 |waves {
                                 |  blockchain.custom.genesis {
                                 |     average-block-delay = 3s
                                 |  }
                                 |  blockchain.custom.functionality {
                                 |    pre-activated-features.1 = 0
                                 |    min-block-time = 3s
                                 |  }
                                 |  miner {
                                 |    enable = no
                                 |  }
                                 |}""".stripMargin)

  val Configs: Seq[Config] = Seq(
    minerConfig.withFallback(Default.head),
    nonMinerConfig.withFallback(Default(1))
  )

  private def getNodeKeyPair(idx: Int): KeyPair =
    KeyPair(PrivateKey(ByteStr.decodeBase58(Default(idx).getString("private-key")).get))
}

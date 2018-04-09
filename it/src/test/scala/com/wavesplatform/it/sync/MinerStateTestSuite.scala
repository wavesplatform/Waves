package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import org.scalatest.{CancelAfterFailure, FunSuite}

import scala.concurrent.duration._

class MinerStateTestSuite extends FunSuite with CancelAfterFailure with NodesFromDocker {
  import MinerStateTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private val transferFee    = 0.001.waves
  private val transferAmount = 1000.waves

  private def miner               = nodes.head
  private def nodeWithZeroBalance = nodes.last

  test("node w/o balance can forge blocks after effective balance increase") {
    val (balance1, eff1)    = nodeWithZeroBalance.accountBalances(nodeWithZeroBalance.address)
    val nodeMinerInfoBefore = nodeWithZeroBalance.debugMinerInfo()
    assert(nodeMinerInfoBefore.isEmpty)
    val txId = miner.transfer(miner.address, nodeWithZeroBalance.address, transferAmount, transferFee).id
    nodes.waitForHeightAriseAndTxPresent(txId)

    val heightAfterTransfer = miner.height

    nodeWithZeroBalance.assertBalances(nodeWithZeroBalance.address, balance1 + transferAmount, eff1 + transferAmount)

    nodeWithZeroBalance.waitForHeight(heightAfterTransfer + 51, 6.minutes) // if you know how to reduce test time, please ping @monroid

    val nodeMinerInfoAfter = nodeWithZeroBalance.debugMinerInfo()
    assert(nodeMinerInfoAfter.nonEmpty)
  }

}

object MinerStateTestSuite {
  import com.wavesplatform.it.NodeConfigs._
  val microblockActivationHeight = 0
  private val minerConfig        = ConfigFactory.parseString(s"""
    |waves {
    |   synchronization {
    |      synchronization-timeout = 10s
    |   }
    |   blockchain {
    |     custom {
    |        functionality {
    |        pre-activated-features = {1=$microblockActivationHeight}
    |        generation-balance-depth-from-50-to-1000-after-height = 100
    |        }
    |        genesis {
    |           average-block-delay = 6s
    |           signature: "zXBp6vpEHgtdsPjVHjSEwMeRiQTAu6DdX3qkJaCRKxgYJk26kazS2XguLYRvL9taHKxrZHNNA7X7LMVFavQzWpT"
    |           transactions = [
    |             {recipient: "3Hm3LGoNPmw1VTZ3eRA2pAfeQPhnaBm6YFC", amount: 250000000000000},
    |             {recipient: "3HZxhQhpSU4yEGJdGetncnHaiMnGmUusr9s", amount: 270000000000000},
    |             {recipient: "3HPG313x548Z9kJa5XY4LVMLnUuF77chcnG", amount: 260000000000000},
    |             {recipient: "3HVW7RDYVkcN5xFGBNAUnGirb5KaBSnbUyB", amount: 2000000000000}
    |           ]
    |        }
    |     }
    |   }
    |   miner.quorum = 0
    |}""".stripMargin)

  private val notMinerConfig = ConfigFactory.parseString(s"""
    |waves {
    |   synchronization {
    |      synchronization-timeout = 10s
    |   }
    |   blockchain {
    |     custom {
    |        functionality {
    |        pre-activated-features = {1=$microblockActivationHeight}
    |        generation-balance-depth-from-50-to-1000-after-height = 100
    |        }
    |        genesis {
    |           average-block-delay = 6s
    |           signature: "zXBp6vpEHgtdsPjVHjSEwMeRiQTAu6DdX3qkJaCRKxgYJk26kazS2XguLYRvL9taHKxrZHNNA7X7LMVFavQzWpT"
    |           transactions = [
    |              {recipient: "3Hm3LGoNPmw1VTZ3eRA2pAfeQPhnaBm6YFC", amount: 250000000000000},
    |              {recipient: "3HZxhQhpSU4yEGJdGetncnHaiMnGmUusr9s", amount: 270000000000000},
    |              {recipient: "3HPG313x548Z9kJa5XY4LVMLnUuF77chcnG", amount: 260000000000000},
    |              {recipient: "3HVW7RDYVkcN5xFGBNAUnGirb5KaBSnbUyB", amount: 2000000000000}
    |          ]
    |        }
    |     }
    |   }
    |  miner.enable = no
    |}""".stripMargin)

  val Configs: Seq[Config] = Seq(
    minerConfig.withFallback(Default.head),
    minerConfig.withFallback(Default(1)),
    notMinerConfig.withFallback(Default(2)),
    notMinerConfig.withFallback(Default(3)),
    minerConfig.withFallback(Default(4)) // node w/o balance
  )

}

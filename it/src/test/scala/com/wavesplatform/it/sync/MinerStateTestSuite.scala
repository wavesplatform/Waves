package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.State
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import org.scalatest.{CancelAfterFailure, FunSuite, Matchers}

import scala.concurrent.duration._

class MinerStateTestSuite extends FunSuite with CancelAfterFailure with NodesFromDocker with Matchers {
  import MinerStateTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private val transferAmount = 1000.waves

  private def miner               = nodes.head
  private def nodeWithZeroBalance = nodes.last

  test("node w/o balance can forge blocks after effective balance increase") {
    val newMinerAddress     = nodeWithZeroBalance.createAddress()
    val (balance1, eff1)    = nodeWithZeroBalance.accountBalances(newMinerAddress)
    val nodeMinerInfoBefore = nodeWithZeroBalance.debugMinerInfo()
    all(nodeMinerInfoBefore) shouldNot matchPattern { case State(`newMinerAddress`, _, ts) if ts > 0 => }
    val txId = miner.transfer(miner.address, newMinerAddress, transferAmount, minFee).id
    nodes.waitForHeightAriseAndTxPresent(txId)

    val heightAfterTransfer = miner.height

    nodeWithZeroBalance.assertBalances(newMinerAddress, balance1 + transferAmount, eff1 + transferAmount)

    nodeWithZeroBalance.waitForHeight(heightAfterTransfer + 51, 6.minutes) // if you know how to reduce test time, please ping @monroid

    val nodeMinerInfoAfter = nodeWithZeroBalance.debugMinerInfo()
    atMost(1, nodeMinerInfoAfter) should matchPattern { case State(`newMinerAddress`, _, ts) if ts > 0 => }
  }
}

object MinerStateTestSuite {
  import com.wavesplatform.it.NodeConfigs._
  private val minerConfig = ConfigFactory.parseString(s"""
    |waves {
    |  synchronization.synchronization-timeout = 10s
    |  blockchain.custom.functionality {
    |    pre-activated-features.1 = 0
    |    generation-balance-depth-from-50-to-1000-after-height = 100
    |  }
    |  miner.quorum = 0
    |}""".stripMargin)

  private val notMinerConfig = ConfigFactory.parseString(s"""
    |waves {
    |  synchronization.synchronization-timeout = 10s
    |  blockchain.custom.functionality {
    |    pre-activated-features.1 = 0
    |    generation-balance-depth-from-50-to-1000-after-height = 100
    |  }
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

package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.State
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import org.scalatest.{FunSuite, Matchers}

import scala.concurrent.duration._

class MinerStateTestSuite extends FunSuite with NodesFromDocker with Matchers {

  import MinerStateTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private val transferAmount = 1000.waves

  private def miner = nodes.head

  private def last = nodes.last

  test("node w/o balance can forge blocks after effective balance increase") {
    val newKeyPair = last.createKeyPair()
    val newAddress = newKeyPair.toAddress.toString

    val bd1                     = miner.balanceDetails(miner.address)
    val minerFullBalanceDetails = miner.balanceDetails(miner.address)
    assert(bd1.regular == minerFullBalanceDetails.available)
    assert(bd1.effective == minerFullBalanceDetails.effective)

    val bd2                  = last.balanceDetails(newAddress)
    val newAccBalanceDetails = last.balanceDetails(newAddress)
    assert(bd2.regular == newAccBalanceDetails.available)
    assert(bd2.effective == newAccBalanceDetails.effective)

    val minerInfoBefore = last.debugMinerInfo()
    all(minerInfoBefore) shouldNot matchPattern { case State(`newAddress`, _, ts) if ts > 0 => }

    miner.waitForPeers(1)
    val txId = miner.transfer(miner.keyPair, newAddress, transferAmount, minFee).id
    nodes.waitForHeightAriseAndTxPresent(txId)

    val heightAfterTransfer = miner.height

    last.assertBalances(newAddress, bd2.regular + transferAmount, bd2.effective + transferAmount)

    last.waitForHeight(heightAfterTransfer + 51, 6.minutes) // if you know how to reduce waiting time, please ping @monroid

    assert(last.balanceDetails(newAddress).generating == bd2.regular + transferAmount)

    val minerInfoAfter = last.debugMinerInfo()
    atMost(1, minerInfoAfter) should matchPattern { case State(`newAddress`, _, ts) if ts > 0 => }

    last.waitForPeers(1)
    val leaseBack = last.lease(newKeyPair, miner.address, (transferAmount - minFee), minFee).id
    nodes.waitForHeightAriseAndTxPresent(leaseBack)

    assert(last.balanceDetails(newAddress).generating == bd2.regular)

    all(miner.debugMinerInfo()) shouldNot matchPattern { case State(`newAddress`, _, ts) if ts > 0 => }

    all(last.debugMinerInfo()) shouldNot matchPattern { case State(`newAddress`, _, ts) if ts > 0 => }

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
    |  blockchain.custom.genesis {
    |     average-block-delay = 5s
    |  }
    |  miner.quorum = 1
    |}""".stripMargin)

  val Configs: Seq[Config] = Seq(
    minerConfig.withFallback(Default.head),
    minerConfig.withFallback(Default(1))
  )

}

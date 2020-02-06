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

  private def miner = nodes.head
  private def last  = nodes.last

  test("node w/o balance can forge blocks after effective balance increase") {
    val newAddress = last.createAddress()

    val (balance1, eff1)        = miner.accountBalances(miner.address)
    val minerFullBalanceDetails = miner.balanceDetails(miner.address)
    assert(balance1 == minerFullBalanceDetails.available)
    assert(eff1 == minerFullBalanceDetails.effective)

    val (balance2, eff2)     = last.accountBalances(newAddress)
    val newAccBalanceDetails = last.balanceDetails(newAddress)
    assert(balance2 == newAccBalanceDetails.available)
    assert(eff2 == newAccBalanceDetails.effective)

    val minerInfoBefore = last.debugMinerInfo()
    all(minerInfoBefore) shouldNot matchPattern { case State(`newAddress`, _, ts) if ts > 0 => }

    miner.waitForPeers(1)
    val txId = miner.transfer(miner.address, newAddress, transferAmount, minFee).id
    nodes.waitForHeightAriseAndTxPresent(txId)

    val heightAfterTransfer = miner.height

    last.assertBalances(newAddress, balance2 + transferAmount, eff2 + transferAmount)

    last.waitForHeight(heightAfterTransfer + 51, 6.minutes) // if you know how to reduce waiting time, please ping @monroid

    assert(last.balanceDetails(newAddress).generating == balance2 + transferAmount)

    val minerInfoAfter = last.debugMinerInfo()
    atMost(1, minerInfoAfter) should matchPattern { case State(`newAddress`, _, ts) if ts > 0 => }

    last.waitForPeers(1)
    val leaseBack = last.lease(newAddress, miner.address, (transferAmount - minFee), minFee).id
    nodes.waitForHeightAriseAndTxPresent(leaseBack)

    assert(last.balanceDetails(newAddress).generating == balance2)

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

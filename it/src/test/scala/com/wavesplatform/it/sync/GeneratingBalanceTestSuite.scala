package com.wavesplatform.it.sync

import com.typesafe.config.Config
import com.wavesplatform.it.api.State
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import org.scalatest.{CancelAfterFailure, FunSuite, Matchers}

import scala.concurrent.duration._

class GeneratingBalanceTestSuite extends FunSuite with CancelAfterFailure with NodesFromDocker with Matchers {
  import MinerStateTestSuite._ //use one config for two tests

  override protected def nodeConfigs: Seq[Config] = Configs

  private val transferAmount = 1000.waves

  private def miner = nodes.head
  private def last  = nodes.last

  test("make full leasing and check generating balance") {
//    val newAddress = last.createAddress()
//
//    val (balance1, eff1)        = miner.accountBalances(miner.address)
//    val minerFullBalanceDetails = miner.balanceDetails(miner.address)
//    assert(balance1 == minerFullBalanceDetails.available)
//    assert(eff1 == minerFullBalanceDetails.effective)
//
//    val (balance2, eff2)     = last.accountBalances(newAddress)
//    val newAccBalanceDetails = last.balanceDetails(newAddress)
//    assert(balance2 == newAccBalanceDetails.available)
//    assert(eff2 == newAccBalanceDetails.effective)
//
//    val minerInfoBefore = last.debugMinerInfo()
//    all(minerInfoBefore) shouldNot matchPattern { case State(`newAddress`, _, ts) if ts > 0 => }
//
//    val txId = miner.transfer(miner.address, newAddress, transferAmount, minFee).id
//    nodes.waitForHeightAriseAndTxPresent(txId)
//
//    val heightAfterTransfer = miner.height
//
//    last.assertBalances(newAddress, balance2 + transferAmount, eff2 + transferAmount)
//
//    last.waitForHeight(heightAfterTransfer + 51, 6.minutes) // if you know how to reduce test time, please ping @monroid
//
//    assert(miner.balanceDetails(miner.address).generating == balance1 - transferAmount - minFee)
//    assert(last.balanceDetails(newAddress).generating == balance2 + transferAmount)
//
//    val minerInfoAfter = last.debugMinerInfo()
//    atMost(1, minerInfoAfter) should matchPattern { case State(`newAddress`, _, ts) if ts > 0 => }
  }
}

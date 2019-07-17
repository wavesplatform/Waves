package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import org.scalatest.{CancelAfterFailure, FunSuite, Matchers}

import scala.concurrent.duration._

class ForgeReturnedToUTXSuite extends FunSuite with CancelAfterFailure with NodesFromDocker with Matchers {

  import ForgeReturnedToUTXSuite._
  override protected def nodeConfigs: Seq[Config] = Configs

  private val transferAmount = 1000.waves

  private def miner = nodes.head
  private def last  = nodes.last

  test("dependent trasactions can be applied if both returned to UTX") {

    val (balance1, eff1)   = miner.accountBalances(miner.address)
    val currentMinerHeight = miner.height

    val assetId = last.issue(last.address, "asset", "descr", issueAmount, 0, false, issueFee, 2, waitForTx = true).id

    miner.waitForHeight(currentMinerHeight + 1)
    val timeToWait = microblockInterval.seconds + 1.second
    val firstTxId = last.transfer(last.address, miner.address, 1L, minFee, Some(assetId), None, 2, waitForTx = true).id
    val secondTxId = miner.transfer(miner.address, last.address, 1L, minFee, Some(assetId), None, 2, waitForTx = true).id

    val firstTxHeight  = last.transactionInfo(firstTxId).height
    val secondTxHeight = last.transactionInfo(secondTxId).height

    secondTxHeight shouldBe firstTxHeight

    miner.waitForHeight(currentMinerHeight + 2)
    miner.waitForTransaction((firstTxId))
    miner.waitForTransaction(secondTxId)

    last.transactionInfo(firstTxId).height shouldBe firstTxHeight + 1
    last.transactionInfo(secondTxId).height shouldBe secondTxHeight + 1

  }

}

object ForgeReturnedToUTXSuite {
  import com.wavesplatform.it.NodeConfigs._

  val microblockInterval  = 3
  private val minerConfig = ConfigFactory.parseString(s"""
                                                         |waves {
                                                         |  miner {
                                                         |    micro-block-interval = ${microblockInterval}s
                                                         |    min-micro-block-age = 17s
                                                         |  }
                                                         |  blockchain.custom.genesis {
                                                         |     average-block-delay = 20s
                                                         |  }
                                                         |  miner.quorum = 1
                                                         |}""".stripMargin)

  val Configs: Seq[Config] = Seq(
    minerConfig.withFallback(Default.head),
    minerConfig.withFallback(Default(1))
  )

}

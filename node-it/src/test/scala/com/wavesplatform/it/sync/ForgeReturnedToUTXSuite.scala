package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import org.scalatest.{CancelAfterFailure, FunSuite, Matchers}

class ForgeReturnedToUTXSuite extends FunSuite with CancelAfterFailure with NodesFromDocker with Matchers {

  import ForgeReturnedToUTXSuite._
  override protected def nodeConfigs: Seq[Config] = Configs

  private def miner = nodes.head
  private def last  = nodes.last

  test("dependent trasactions can be added to UTX if first mined and returned to UTX") {

    //asset tx should be mined in first microblock as as new keyblock mined, others microblocks should not be applied due to big microblockInterval
    val assetId                      = last.issue(last.address, "asset", "descr", issueAmount, 0, reissuable = false, issueFee, waitForTx = true).id
    val issueAssetInitialHeight: Int = last.transactionInfo(assetId).height

    //all microblocks should returned to utx, assetId should be returned to UTX and no any microblocks will be mined on this height
    //so trasfer tx will stay in utx until new keyblock mined
    val transferTx = last.transfer(last.address, miner.address, 1L, minFee, Some(assetId), None, waitForTx = true).id

    val issueAssetHeight = last.transactionInfo(assetId).height
    val transferTxHeight = last.transactionInfo(transferTx).height

    //trasfer tx and issue asset tx should be placed in the same microblock
    transferTxHeight shouldBe issueAssetHeight
    transferTxHeight shouldNot be(issueAssetInitialHeight)

  }

}

object ForgeReturnedToUTXSuite {
  import com.wavesplatform.it.NodeConfigs._

  //microblock interval should be greater than avarage block interval
  val microblockInterval  = 60
  private val minerConfig = ConfigFactory.parseString(s"""
                                                         |waves {
                                                         |  miner {
                                                         |    micro-block-interval = ${microblockInterval}s
                                                         |    min-micro-block-age = 60s
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

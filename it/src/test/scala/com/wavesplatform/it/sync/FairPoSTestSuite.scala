package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.{CancelAfterFailure, FunSuite}
import com.wavesplatform.it.api.State
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import scala.concurrent.duration._

class FairPoSTestSuite extends FunSuite with CancelAfterFailure with NodesFromDocker {
  import FairPoSTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private val transferFee    = 0.001.waves
  private val transferAmount = 1000.waves

  test("blockchain grows with FairPoS activated") {
    nodes.head.waitForHeight(10, 3.minutes)

    val txId = nodes.head.transfer(nodes.head.address, nodes.last.address, transferAmount, transferFee).id
    nodes.last.waitForTransaction(txId)

    val heightAfterTransfer = nodes.head.height

    nodes.head.waitForHeight(heightAfterTransfer + 30, 5.minutes)
  }
//
//  test("FairPoS just works") {
//    val startHeight = nodes.head.height
//    nodes.head.waitForHeight(startHeight + 99, 5.minutes)
//    val stopHeight = nodes.head.height
//
//    val minedBlocks = nodes.head.blockHeadersSeq(startHeight, stopHeight).map(_.generator)
//    val actualDistribution = minedBlocks.groupBy(identity).mapValues(_.size.toDouble / minedBlocks.size.toDouble)
//
//    val nodesBalances = nodes.map(_.address).map(a => a -> nodes.head.accountBalance(a)).toMap
//    val totalBalance = nodesBalances.values.sum.toDouble
//    val expectedDistribution = nodesBalances.mapValues(_.toDouble / totalBalance)
//
//    val x: Map[(String, Double), (String, Double)] = expectedDistribution.zip(actualDistribution)
//  }
}

object FairPoSTestSuite {
  import com.wavesplatform.it.NodeConfigs._
  private val microblockActivationHeight = 0
  private val fairPoSActivationHeight    = 10

  private val config =
    ConfigFactory.parseString(s"""
    |waves {
    |   blockchain.custom {
    |      functionality {
    |        pre-activated-features {1 = $microblockActivationHeight, 8 = $fairPoSActivationHeight}
    |        generation-balance-depth-from-50-to-1000-after-height = 1000
    |      }
    |   }
    |   miner.quorum = 1
    |}""".stripMargin)

  val Configs: Seq[Config] = Default.map(config.withFallback(_)).take(4)
}

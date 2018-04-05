package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import org.scalatest.{CancelAfterFailure, FunSuite}

import scala.concurrent.duration._

class FairPoSTestSuite extends FunSuite with CancelAfterFailure with NodesFromDocker {
  import FairPoSTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private val transferFee    = 0.001.waves
  private val transferAmount = 1000.waves

  test("blockchain grows with FairPoS activated") {
    nodes.head.waitForHeight(10, 2.minutes)

    val txId = nodes.head.transfer(nodes.head.address, nodes.last.address, transferAmount, transferFee).id
    nodes.waitForHeightAriseAndTxPresent(txId)

    val heightAfterTransfer = nodes.head.height

    nodes.head.waitForHeight(heightAfterTransfer + 30, 3.minutes)
  }
}

object FairPoSTestSuite {
  import com.wavesplatform.it.NodeConfigs._
  private val microblockActivationHeight = 0
  private val fairPoSActivationHeight    = 10
  private val ts                         = System.currentTimeMillis()

  private val config =
    ConfigFactory.parseString(s"""
    |waves {
    |   blockchain.custom {
    |      functionality {
    |        pre-activated-features {1 = $microblockActivationHeight, 7 = $fairPoSActivationHeight}
    |        generation-balance-depth-from-50-to-1000-after-height = 100
    |      }
    |      genesis {
    |         average-block-delay = 6s
    |         timestamp = $ts
    |         signature = null
    |         transactions = [
    |            {recipient: "3Hm3LGoNPmw1VTZ3eRA2pAfeQPhnaBm6YFC", amount: 250000000000000},
    |            {recipient: "3HPG313x548Z9kJa5XY4LVMLnUuF77chcnG", amount: 250000000000000},
    |            {recipient: "3HZxhQhpSU4yEGJdGetncnHaiMnGmUusr9s", amount: 250000000000000},
    |            {recipient: "3HVW7RDYVkcN5xFGBNAUnGirb5KaBSnbUyB", amount: 250000000000000}
    |         ]
    |      }
    |   }
    |   miner.quorum = 1
    |}""".stripMargin)

  val Configs: Seq[Config] =
    Seq(config.withFallback(Default.head), config.withFallback(Default(1)), config.withFallback(Default(2)), config.withFallback(Default(3)))
}

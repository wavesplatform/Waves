package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.transactions.NodesFromDocker
import org.scalatest.{CancelAfterFailure, FunSuite, Matchers}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.util._

import scala.concurrent.duration._

class PamPamTestSuite extends FunSuite with Matchers with CancelAfterFailure with NodesFromDocker {

  private val transferFee = 0.001.waves

  private val miner1Balance = 1000.waves
  private val miner2Balance = 10000.waves
  private val miner3Balance = 10000000.waves

  private val baseTarget = 100

  test("FairPoS mining") {
    println("123123123")
    val miner1 = nodes.head
    val miner2 = nodes(1)
    val miner3 = nodes(2)
    val miner4 = nodes(3)

    val txId = nodes.head.transfer(nodes.head.address, nodes.last.address, transferAmount, transferFee).id
    miner1.getGeneratedBlocks(miner1.address, 100, 200)

    println("a")
    nodes.head.waitForHeight(10, 1.minutes)
    println("b")

    nodes
      .map(_.debugMinerInfo())
      .foreach(println)

    println("c")

    true shouldBe true
  }
//
//  def waitForBlock = {
//    nodes.head.waitForHeight()
//  }

  def getMinedBlocks()

  override protected def nodeConfigs: Seq[Config] = PamPamTestSuite.Configs(baseTarget, miner3Balance)
}

object PamPamTestSuite {
  import com.wavesplatform.it.NodeConfigs._
  private val microblockActivationHeight = 0
  private val fairPoSActivationHeight    = 10

  private def config(target: Long, balance: Long) =
    ConfigFactory.parseString(s"""
                                 |waves {
                                 |   blockchain.custom {
                                 |      functionality {
                                 |        pre-activated-features {1 = $microblockActivationHeight, 8 = $fairPoSActivationHeight}
                                 |        generation-balance-depth-from-50-to-1000-after-height = 1000
                                 |      }
                                 |      genesis {
                                 |        average-block-delay: 10000ms
                                 |        initial-base-target: 200000
                                 |        timestamp: 1489352400000
                                 |        block-timestamp: 1489352400000
                                 |        signature: "3o2e8WaLuwc7zFbtCCsHKSvXYQiDFeeYyiRG1CMnGMKBpcU2oEUxJ9sVeUNyNTsoAcFeXmofPeLGUHsHkkNQSYR9"
                                 |        initial-balance: 1000000000000000
                                 |        transactions = [
                                 |          {recipient: "3Hm3LGoNPmw1VTZ3eRA2pAfeQPhnaBm6YFC", amount: 1400000000000},
                                 |          {recipient: "3HZxhQhpSU4yEGJdGetncnHaiMnGmUusr9s", amount: 1000},
                                 |          {recipient: "3HPG313x548Z9kJa5XY4LVMLnUuF77chcnG", amount: 1500},
                                 |          {recipient: "3HVW7RDYVkcN5xFGBNAUnGirb5KaBSnbUyB", amount: 1600}
                                 |        ]
                                 |      }
                                 |   }
                                 |   miner.quorum = 1
                                 |}""".stripMargin)

  def Configs(target: Long, balance: Long): Seq[Config] = Default.map(config(target, balance).withFallback(_)).take(4)
}

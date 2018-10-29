package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.{ReportingTestName, WaitForHeight2}
import org.scalatest.{CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.duration._
import com.wavesplatform.it.{TransferSending}

import scala.concurrent.Await

class MinerTestTestSuite
    extends FreeSpec
    with Matchers
    with WaitForHeight2
    with CancelAfterFailure
    with ReportingTestName
    with NodesFromDocker
    with TransferSending {
  import MinerTestTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs
  private val nodeAddresses                       = nodeConfigs.map(_.getString("address")).toSet

  "mining test" in {
    Await.result(processRequests(generateTransfersToRandomAddresses(10, nodeAddresses)), 2.minutes)

    nodes.waitForHeightArise()

    1 to 10 foreach (_ => {
      Await.result(processRequests(generateTransfersToRandomAddresses(10, nodeAddresses)), 2.minutes)

      Thread.sleep(5000)

      nodes.waitForHeightArise()
    })

    nodes.waitForHeightArise()

  }
}

object MinerTestTestSuite {
  import com.wavesplatform.it.NodeConfigs._
  private val FirstNode = ConfigFactory.parseString(s"""
                                                       |waves {
                                                       |  utx.allow-transactions-from-smart-accounts = false
                                                       |  miner {
                                                       |      quorum = 0
                                                       |      enable = yes
                                                       |  }
                                                       |}""".stripMargin)

  private val SecondNode = ConfigFactory.parseString(s"""
                                                        |waves {
                                                        |  utx.allow-transactions-from-smart-accounts = true
                                                        |  miner {
                                                        |      enable = no
                                                        |  }
                                                        |}""".stripMargin)

  val Configs: Seq[Config] = Seq(
    FirstNode.withFallback(Default.head),
    SecondNode.withFallback(Default(1))
  )

}

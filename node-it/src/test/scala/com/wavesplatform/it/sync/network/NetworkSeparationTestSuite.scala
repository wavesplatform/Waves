package com.wavesplatform.it.sync.network

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{issueAmount, issueFee, minFee}
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.{Docker, ReportingTestName, WaitForHeight2}
import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent.duration._

class NetworkSeparationTestSuite
    extends FreeSpec
    with Matchers
    with WaitForHeight2
    with ReportingTestName
    with NodesFromDocker {
  import NetworkSeparationTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private def nodeA: Docker.DockerNode = dockerNodes().head
  private def nodeB: Docker.DockerNode = dockerNodes().last

  "node should grow up to 10 blocks together and sync" in {
    nodes.waitForSameBlockHeadersAt(10)
  }

  // Doing all work in one step, because nodes will not be available for requests and ReportingTestName fails here
  "then we disconnect nodes from the network, wait some time and connect them again" in {
    val lastMaxHeight = nodes.map(_.height).max
    dockerNodes().foreach(docker.disconnectFromNetwork)
    Thread.sleep(80.seconds.toMillis) // >= 10 blocks, because a new block appears every 6 seconds
    docker.connectToNetwork(dockerNodes(): _*)
    nodes.map(_.height).max shouldBe >=(lastMaxHeight + 5)
  }

  "nodes should sync" in {
    val maxHeight = nodes.map(_.height).max
    log.debug(s"Max height is $maxHeight")
    nodes.waitForSameBlockHeadersAt(maxHeight + 5)
  }

  "after fork node should apply correct subchain" in {
    val issuedAssetId = nodeA.issue(nodeA.keyPair, "TestAsset", "description", issueAmount, 8, reissuable = true, issueFee).id
    nodes.waitForHeightAriseAndTxPresent(issuedAssetId)

    nodeA.assertAssetBalance(nodeA.address, issuedAssetId, issueAmount)

    val txId = nodeA.transfer(nodeA.keyPair, nodeB.address, issueAmount / 2, minFee, Some(issuedAssetId)).id
    nodes.waitForHeightAriseAndTxPresent(txId)

    docker.disconnectFromNetwork(nodeA)

    val burnNoOwnerTxTd = nodeB.burn(nodeB.keyPair, issuedAssetId, issueAmount / 2, minFee).id
    val heightAfter = nodeB.waitForTransaction(burnNoOwnerTxTd).height

    Thread.sleep(60.seconds.toMillis)
    docker.disconnectFromNetwork(nodeB)
    docker.connectToNetwork(nodeA)

    nodeA.waitForHeight(heightAfter + 1)

    docker.connectToNetwork(nodeB)
    Thread.sleep(80.seconds.toMillis)

    val height = nodeA.height
    assert(nodeA.blockAt(height) != nodeB.blockAt(height))
  }

}

object NetworkSeparationTestSuite {
  import com.wavesplatform.it.NodeConfigs._
  private val woFeatureConfig = ConfigFactory.parseString(s"""
                                                            |waves {
                                                            |  synchronization.synchronization-timeout = 10s
                                                            |  blockchain.custom.functionality {
                                                            |    pre-activated-features = {
                                                            |     1 = 0
                                                            |     6 = 100
                                                            |     }
                                                            |  }
                                                            |  miner.quorum = 0 
                                                            |}""".stripMargin)

  private val withFeatureConfig = ConfigFactory.parseString(s"""
                                                               |waves {
                                                               |  synchronization.synchronization-timeout = 10s
                                                               |  blockchain.custom.functionality {
                                                               |    pre-activated-features = {
                                                               |    1 = 0
                                                               |    6 = 0
                                                               |    }
                                                               |  }
                                                               |  miner.quorum = 0
                                                               |}""".stripMargin)

  val Configs: Seq[Config] = Seq(
    woFeatureConfig.withFallback(Miners.head),
    withFeatureConfig.withFallback(Miners.last)
  )
}

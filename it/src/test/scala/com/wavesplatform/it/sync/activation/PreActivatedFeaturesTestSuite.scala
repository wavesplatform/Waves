package com.wavesplatform.it.sync.activation

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.features.api.NodeFeatureStatus
import com.wavesplatform.features.{BlockchainFeatureStatus, BlockchainFeatures}
import com.wavesplatform.it.ReportingTestName
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import org.scalatest.{CancelAfterFailure, FreeSpec, Matchers}

class PreActivatedFeaturesTestSuite
    extends FreeSpec
    with Matchers
    with CancelAfterFailure
    with NodesFromDocker
    with ActivationStatusRequest
    with ReportingTestName {

  override protected def nodeConfigs: Seq[Config] = PreActivatedFeaturesTestSuite.Configs
  nodes.foreach(n => n.accountBalances(n.address))

  "before activation check" in {
    nodes.waitForHeight(PreActivatedFeaturesTestSuite.votingInterval / 2)

    val mainNodeStatus = nodes.head.featureActivationStatus(PreActivatedFeaturesTestSuite.featureNum)
    mainNodeStatus.description shouldBe PreActivatedFeaturesTestSuite.featureDescr
    assertVotingStatus(mainNodeStatus, mainNodeStatus.supportingBlocks.get, BlockchainFeatureStatus.Undefined, NodeFeatureStatus.Voted)

    val otherNodes = nodes.tail.map(_.featureActivationStatus(PreActivatedFeaturesTestSuite.featureNum))
    otherNodes.foreach { s =>
      s.description shouldBe PreActivatedFeaturesTestSuite.featureDescr
      assertActivatedStatus(s, 0, NodeFeatureStatus.Voted)
    }
  }

  "on activation height check" in {
    nodes.waitForHeight(PreActivatedFeaturesTestSuite.votingInterval + 4)

    val mainNodeStatus = nodes.head.featureActivationStatus(PreActivatedFeaturesTestSuite.featureNum)
    mainNodeStatus.description shouldBe PreActivatedFeaturesTestSuite.featureDescr
    assertApprovedStatus(mainNodeStatus, PreActivatedFeaturesTestSuite.votingInterval * 2, NodeFeatureStatus.Voted)

    val otherNodes = nodes.tail.map(_.featureActivationStatus(PreActivatedFeaturesTestSuite.featureNum))
    otherNodes.foreach { s =>
      s.description shouldBe PreActivatedFeaturesTestSuite.featureDescr
      assertActivatedStatus(s, 0, NodeFeatureStatus.Voted)
    }
  }

  "after activation height check" in {
    nodes.waitForHeight(PreActivatedFeaturesTestSuite.votingInterval * 2 + 4)

    val mainNodeStatus = nodes.head.featureActivationStatus(PreActivatedFeaturesTestSuite.featureNum)
    mainNodeStatus.description shouldBe PreActivatedFeaturesTestSuite.featureDescr
    assertActivatedStatus(mainNodeStatus, PreActivatedFeaturesTestSuite.votingInterval * 2, NodeFeatureStatus.Voted)

    val otherNodes = nodes.tail.map(_.featureActivationStatus(PreActivatedFeaturesTestSuite.featureNum))
    otherNodes.foreach { s =>
      s.description shouldBe PreActivatedFeaturesTestSuite.featureDescr
      assertActivatedStatus(s, 0, NodeFeatureStatus.Voted)
    }
  }

}

object PreActivatedFeaturesTestSuite {
  import com.wavesplatform.it.NodeConfigs._
  val votingInterval = 10

  val featureNum: Short = BlockchainFeatures.DataTransaction.id
  val featureDescr      = BlockchainFeatures.DataTransaction.description

  private val supportedConfig = ConfigFactory.parseString(s"""waves {
                                                             |  blockchain.custom.functionality {
                                                             |    pre-activated-features = null
                                                             |    feature-check-blocks-period = $votingInterval
                                                             |    blocks-for-feature-activation = 1
                                                             |  }
                                                             |  features.supported = [$featureNum]
                                                             |  miner.quorum = 1
                                                             |}""".stripMargin)

  private val preactivatedConfig = ConfigFactory.parseString(s"""waves {
                                                                |  blockchain.custom.functionality {
                                                                |  pre-activated-features {
                                                                |        1: 100
                                                                |        2: 100
                                                                |        3: 100
                                                                |        4: 100
                                                                |        5: 0
                                                                |        6: 100
                                                                |        7: 100
                                                                |        8: 100
                                                                |        9: 100
                                                                |        10: 100
                                                                |        11: 100
                                                                |      }
                                                                |  }
                                                                |  features.supported = [$featureNum]
                                                                |  miner.quorum = 1
                                                                |}""".stripMargin)

  val Configs: Seq[Config] = Seq(
    supportedConfig.withFallback(Default.last),
    preactivatedConfig.withFallback(Default.head),
    preactivatedConfig.withFallback(Default(1))
  )

}

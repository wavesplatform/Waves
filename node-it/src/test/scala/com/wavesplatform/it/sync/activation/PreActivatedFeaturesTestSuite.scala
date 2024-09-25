package com.wavesplatform.it.sync.activation
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.features.api.NodeFeatureStatus
import com.wavesplatform.features.{BlockchainFeatureStatus, BlockchainFeatures}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.{BaseFreeSpec, Docker}

class PreActivatedFeaturesTestSuite extends BaseFreeSpec with ActivationStatusRequest {
  override protected def nodeConfigs: Seq[Config] = PreActivatedFeaturesTestSuite.Configs

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    nodes.foreach(n => n.accountBalances(n.address))
  }

  "before activation check" in {
    nodes.waitForHeight(PreActivatedFeaturesTestSuite.votingInterval / 2)

    val mainNodeStatus = nodes.head.featureActivationStatus(PreActivatedFeaturesTestSuite.featureNum)
    mainNodeStatus.description shouldBe PreActivatedFeaturesTestSuite.featureDescr
    assertVotingStatus(mainNodeStatus, mainNodeStatus.supportingBlocks.get, BlockchainFeatureStatus.Undefined, NodeFeatureStatus.Voted)

    val otherNodes = nodes.tail.map(_.featureActivationStatus(PreActivatedFeaturesTestSuite.featureNum))
    otherNodes.foreach { s =>
      s.description shouldBe PreActivatedFeaturesTestSuite.featureDescr
      assertActivatedStatus(s, 0, NodeFeatureStatus.Implemented)
    }
  }
  "on activation height check" in {
    nodes.waitForHeight(PreActivatedFeaturesTestSuite.votingInterval + 3)

    val mainNodeStatus = nodes.head.featureActivationStatus(PreActivatedFeaturesTestSuite.featureNum)
    mainNodeStatus.description shouldBe PreActivatedFeaturesTestSuite.featureDescr
    mainNodeStatus.blockchainStatus shouldBe BlockchainFeatureStatus.Undefined
    mainNodeStatus.activationHeight shouldBe None
    mainNodeStatus.supportingBlocks shouldBe Some(0)

    val otherNodes = nodes.tail
    otherNodes.foreach { node =>
      val feature = node.featureActivationStatus(PreActivatedFeaturesTestSuite.featureNum)
      feature.description shouldBe PreActivatedFeaturesTestSuite.featureDescr
      assertActivatedStatus(feature, 0, NodeFeatureStatus.Implemented)

      val node1    = docker.restartNode(node.asInstanceOf[Docker.DockerNode])
      val feature2 = node1.featureActivationStatus(PreActivatedFeaturesTestSuite.featureNum)
      assertActivatedStatus(feature2, 0, NodeFeatureStatus.Implemented)
    }
  }
  "after activation height check" in {
    nodes.waitForHeight(PreActivatedFeaturesTestSuite.votingInterval * 2 + 4)

    val mainNodeStatus = nodes.head.featureActivationStatus(PreActivatedFeaturesTestSuite.featureNum)
    mainNodeStatus.description shouldBe PreActivatedFeaturesTestSuite.featureDescr
    assertUndefinedStatus(mainNodeStatus, NodeFeatureStatus.Voted)

    val otherNodes = nodes.tail.map(_.featureActivationStatus(PreActivatedFeaturesTestSuite.featureNum))
    otherNodes.foreach { s =>
      s.description shouldBe PreActivatedFeaturesTestSuite.featureDescr
      assertActivatedStatus(s, 0, NodeFeatureStatus.Implemented)
    }
  }
}

object PreActivatedFeaturesTestSuite {
  import com.wavesplatform.it.NodeConfigs._
  val votingInterval    = 10
  val featureNum: Short = BlockchainFeatures.SmallerMinimalGeneratingBalance.id
  val featureDescr      = BlockchainFeatures.SmallerMinimalGeneratingBalance.description
  private val supportedConfig = ConfigFactory.parseString(s"""waves {
                                                             |  blockchain.custom.functionality {
                                                             |    pre-activated-features = {}
                                                             |    feature-check-blocks-period = $votingInterval
                                                             |    blocks-for-feature-activation = 1
                                                             |  }
                                                             |  features.supported = [$featureNum]
                                                             |  miner.quorum = 1
                                                             |}""".stripMargin)
  private val preactivatedConfig = ConfigFactory.parseString(s"""waves {
                                                                |  blockchain.custom.functionality {
                                                                |  feature-check-blocks-period = $votingInterval
                                                                |  pre-activated-features {
                                                                |        1 = 0
                                                                |        2 = 100
                                                                |        3 = 100
                                                                |        4 = 100
                                                                |        5 = 100
                                                                |        6 = 100
                                                                |        7 = 100
                                                                |        8 = 100
                                                                |        9 = 100
                                                                |        10 = 100
                                                                |        11 = 100
                                                                |        15 = 0
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

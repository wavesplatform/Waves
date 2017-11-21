package com.wavesplatform.it.activation

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.features.BlockchainFeatureStatus
import com.wavesplatform.features.api.NodeFeatureStatus
import com.wavesplatform.it.{Docker, Node, NodeConfigs, ReportingTestName}
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.Await

class VoteForFeatureByDefaultTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with CancelAfterFailure
  with ActivationStatusRequest with ReportingTestName {

  import VoteForFeatureByDefaultTestSuite._

  private lazy val docker = Docker(getClass)
  override lazy val nodes: Seq[Node] = docker.startNodes(Configs)

  val defaultVotingFeatureNum: Short = 1

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    nodes.foreach(_.status) // Initialize
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    docker.close()
  }

  "supported blocks increased when voting starts, one node votes against, three by default" in {
    val checkHeight: Int = votingInterval * 2 / 3

    val activationInfo = activationStatus(Seq(nodes.last, nodes.head), checkHeight, defaultVotingFeatureNum, 4.minute)
    val supportedNodeActivationInfo = activationInfo(nodes.last)
    val nonSupportedNodeActivationInfo = activationInfo(nodes.head)

    val generatedBlocks = Await.result(nodes.last.blockSeq(1, checkHeight), 2.minute)
    val featuresMapInGeneratedBlocks = generatedBlocks.flatMap(b => b.features.getOrElse(Seq.empty)).groupBy(x => x)
    val votesForFeature1 = featuresMapInGeneratedBlocks.getOrElse(defaultVotingFeatureNum, Seq.empty).length

    assertVotingStatus(supportedNodeActivationInfo, votesForFeature1, BlockchainFeatureStatus.Undefined, NodeFeatureStatus.Voted)
    assertVotingStatus(nonSupportedNodeActivationInfo, votesForFeature1, BlockchainFeatureStatus.Undefined, NodeFeatureStatus.Implemented)
  }

  "blockchain status is APPROVED in second voting interval, one node votes against, three by default" in {
    val checkHeight: Int = votingInterval * 2 - blocksForActivation / 2

    val supportedNodeActivationInfo = activationStatus(nodes.last, checkHeight, defaultVotingFeatureNum, 3.minute)
    assertApprovedStatus(supportedNodeActivationInfo, votingInterval * 2, NodeFeatureStatus.Voted)
  }

  "blockchain status is ACTIVATED in the end of second voting interval, one node votes against, three by default" in {
    val checkHeight: Int = votingInterval * 2

    val supportedNodeActivationInfo = activationStatus(nodes.last, checkHeight, defaultVotingFeatureNum, 2.minute)
    assertActivatedStatus(supportedNodeActivationInfo, checkHeight, NodeFeatureStatus.Voted)
  }


  object VoteForFeatureByDefaultTestSuite {

    import NodeConfigs.Default

    val votingInterval = 25
    val blocksForActivation = 18
    val defaultVotingFeatureNum: Short = 1
    val nonVotingFeatureNum: Short = 2

    private val baseConfig = ConfigFactory.parseString(
      s"""waves {
         |  blockchain.custom {
         |    functionality {
         |      pre-activated-features = {}
         |      feature-check-blocks-period = $votingInterval
         |      blocks-for-feature-activation = $blocksForActivation
         |    }
         |    genesis {
         |      signature: "zXBp6vpEHgtdsPjVHjSEwMeRiQTAu6DdX3qkJaCRKxgYJk26kazS2XguLYRvL9taHKxrZHNNA7X7LMVFavQzWpT"
         |      transactions = [
         |        {recipient: "3Hm3LGoNPmw1VTZ3eRA2pAfeQPhnaBm6YFC", amount: 250000000000000},
         |        {recipient: "3HZxhQhpSU4yEGJdGetncnHaiMnGmUusr9s", amount: 270000000000000},
         |        {recipient: "3HPG313x548Z9kJa5XY4LVMLnUuF77chcnG", amount: 260000000000000},
         |        {recipient: "3HVW7RDYVkcN5xFGBNAUnGirb5KaBSnbUyB", amount: 2000000000000}
         |      ]
         |    }
         |  }
         |  miner.quorum = 3
         |}""".stripMargin
    )

    private val supportedNodes = ConfigFactory
      .parseString(s"waves.features.supported=[$defaultVotingFeatureNum]")
      .withFallback(baseConfig)

    private val nonSupportedNodes = ConfigFactory
      .parseString(s"waves.features.supported=[$nonVotingFeatureNum]")
      .withFallback(baseConfig)

    val Configs: Seq[Config] = Seq(
      nonSupportedNodes.withFallback(Default(3)),
      supportedNodes.withFallback(Default(1)),
      supportedNodes.withFallback(Default(2)),
      supportedNodes.withFallback(Default.head)
    )

    val NodesCount: Int = Configs.length

  }

}
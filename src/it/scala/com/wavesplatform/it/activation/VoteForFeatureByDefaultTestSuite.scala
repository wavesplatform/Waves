package com.wavesplatform.it.activation

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.features.BlockchainFeatureStatus
import com.wavesplatform.features.api.NodeFeatureStatus
import com.wavesplatform.it.{Docker, NodeConfigs}
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class VoteForFeatureByDefaultTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with CancelAfterFailure with ActivationStatusRequest {

  import VoteForFeatureByDefaultTestSuite._

  private val docker = Docker(getClass)
  private val nodes = docker.startNodes(Configs)
  val defaultVotingFeatureNum: Short = 1


  override protected def beforeAll(): Unit = {
    super.beforeAll()
    Await.result(Future.traverse(nodes)(_.waitForPeers(NodesCount - 1)), 2.minute)
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    docker.close()
  }

  "wait nodes are synchronized" in Await.result(
    for {
      _ <- Future.traverse(nodes)(_.waitForHeight(votingInterval / 2))
      blocks <- Future.traverse(nodes)(_.blockAt(votingInterval / 3))
    } yield all(blocks.map(_.signature)) shouldBe blocks.head.signature,
    5.minutes
  )

  "supported blocks increased when voting starts, one node votes against, three by default" in {
    val checkHeight: Int = votingInterval * 2 / 3
    val supportedNodeActivationInfo = activationStatus(nodes.last, checkHeight, defaultVotingFeatureNum, 4.minute)
    val nonSupportedNodeActivationInfo = activationStatus(nodes.head, checkHeight, defaultVotingFeatureNum, 4.minute)

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

    val votingInterval = 20
    val blocksForActivation = 15
    val defaultVotingFeatureNum: Short = 1
    val nonVotingFeatureNum: Short = 2

    private val baseConfig = ConfigFactory.parseString(
      s"""waves.blockchain.custom {
         |  functionality {
         |    pre-activated-features = {}
         |    feature-check-blocks-period = $votingInterval
         |    blocks-for-feature-activation = $blocksForActivation
         |  }
         |  genesis {
         |    signature: "zXBp6vpEHgtdsPjVHjSEwMeRiQTAu6DdX3qkJaCRKxgYJk26kazS2XguLYRvL9taHKxrZHNNA7X7LMVFavQzWpT"
         |    transactions = [
         |      {recipient: "3Hm3LGoNPmw1VTZ3eRA2pAfeQPhnaBm6YFC", amount: 250000000000000},
         |      {recipient: "3HZxhQhpSU4yEGJdGetncnHaiMnGmUusr9s", amount: 270000000000000},
         |      {recipient: "3HPG313x548Z9kJa5XY4LVMLnUuF77chcnG", amount: 260000000000000},
         |      {recipient: "3HVW7RDYVkcN5xFGBNAUnGirb5KaBSnbUyB", amount: 2000000000000}
         |    ]
         |  }
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
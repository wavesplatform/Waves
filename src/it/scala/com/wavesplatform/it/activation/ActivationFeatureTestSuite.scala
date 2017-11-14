package com.wavesplatform.it.activation

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.features.BlockchainFeatureStatus
import com.wavesplatform.features.api.NodeFeatureStatus
import com.wavesplatform.it.{Docker, Node, NodeConfigs}
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

class ActivationFeatureTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with CancelAfterFailure
  with ActivationStatusRequest {

  import ActivationFeatureTestSuite._

  private val waitCompletion = 6.minutes
  private lazy val docker = Docker(getClass)
  private lazy val nodes: Seq[Node] = docker.startNodes(Configs)

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    Await.result(Future.traverse(nodes)(_.waitForPeers(NodesCount - 1)), waitCompletion)
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    docker.close()
  }

  "supported blocks increased when voting starts" in {
    val checkHeight: Int = votingInterval * 2 / 3

    val activationStatusWhileVoting = activationStatus(nodes, checkHeight, featureNum, waitCompletion)
    val activationStatusIntervalLastVotingBlock = activationStatus(nodes, votingInterval, featureNum, waitCompletion)

    val generatedBlocks = Await.result(nodes.head.blockSeq(1, checkHeight), waitCompletion)
    val featuresMapInGeneratedBlocks = generatedBlocks.flatMap(b => b.features.getOrElse(Seq.empty)).groupBy(x => x)
    val votesForFeature1 = featuresMapInGeneratedBlocks.getOrElse(featureNum, Seq.empty).length

    activationStatusWhileVoting.foreach { case (n, info) =>
      withClue(n.settings.networkSettings.nodeName) {
        assertVotingStatus(info, votesForFeature1, BlockchainFeatureStatus.Undefined, NodeFeatureStatus.Voted)
      }
    }

    activationStatusIntervalLastVotingBlock.foreach { case (n, info) =>
      assertVotingStatus(info, blocksForActivation - 1, BlockchainFeatureStatus.Undefined, NodeFeatureStatus.Voted)
    }
  }

  "supported blocks counter resets on the next voting interval" in {
    val checkHeight: Int = votingInterval * 2 - blocksForActivation / 2
    activationStatus(nodes, checkHeight, featureNum, waitCompletion).foreach { case (n, info) =>
      withClue(n.settings.networkSettings.nodeName) {
        info.supportedBlocks.get shouldBe blocksForActivation / 2
        info.blockchainStatus shouldBe BlockchainFeatureStatus.Undefined
      }
    }
  }

  "blockchain status is APPROVED in second voting interval" in {
    val checkHeight: Int = votingInterval * 2
    activationStatus(nodes, checkHeight, featureNum, waitCompletion).foreach { case (n, info) =>
      withClue(n.settings.networkSettings.nodeName) {
        // Activation will be on a next voting interval
        assertApprovedStatus(info, checkHeight + votingInterval, NodeFeatureStatus.Voted)
      }
    }
  }

  "blockchain status is ACTIVATED in third voting interval" in {
    val checkHeight: Int = votingInterval * 3
    activationStatus(nodes, checkHeight, featureNum, waitCompletion).foreach { case (n, info) =>
      withClue(n.settings.networkSettings.nodeName) {
        assertActivatedStatus(info, checkHeight, NodeFeatureStatus.Voted)
      }
    }
  }


  object ActivationFeatureTestSuite {

    val NodesCount: Int = 4

    val votingInterval = 12
    val blocksForActivation = 12 // should be even
    val featureNum: Short = 1

    private val supportedNodes = ConfigFactory.parseString(
      s"""waves {
         |  blockchain {
         |    custom {
         |      functionality {
         |        pre-activated-features = {}
         |        feature-check-blocks-period = $votingInterval
         |        blocks-for-feature-activation = $blocksForActivation
         |      }
         |    }
         |  }
         |  features.supported = [$featureNum]
         |  miner.quorum = ${NodesCount - 1}
         |}""".stripMargin
    )

    val Configs: Seq[Config] = Random.shuffle(NodeConfigs.Default.init).take(NodesCount).map(supportedNodes.withFallback(_))

  }

}
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

  private val waitCompletion = 4.minutes
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

    val activationStatusWhileVoting = activationStatus(nodes.head, checkHeight, featureNum, waitCompletion)
    val activationStatusIntervalLastVotingBlock = activationStatus(nodes.head, votingInterval, featureNum, waitCompletion)

    val generatedBlocks = Await.result(nodes.head.blockSeq(1, checkHeight), waitCompletion)
    val featuresMapInGeneratedBlocks = generatedBlocks.flatMap(b => b.features.getOrElse(Seq.empty)).groupBy(x => x)
    val votesForFeature1 = featuresMapInGeneratedBlocks.getOrElse(featureNum, Seq.empty).length

    assertVotingStatus(activationStatusWhileVoting, votesForFeature1,
      BlockchainFeatureStatus.Undefined, NodeFeatureStatus.Voted)

    assertVotingStatus(activationStatusIntervalLastVotingBlock, blocksForActivation - 1,
      BlockchainFeatureStatus.Undefined, NodeFeatureStatus.Voted)
  }

  "supported blocks counter resets on the next voting interval" in {
    val checkHeight: Int = votingInterval * 2 - blocksForActivation / 2
    val activationStatusInfo = activationStatus(nodes.last, checkHeight, featureNum, waitCompletion)

    activationStatusInfo.supportedBlocks.get shouldBe blocksForActivation / 2
    activationStatusInfo.blockchainStatus shouldBe BlockchainFeatureStatus.Undefined
  }

  "blockchain status is APPROVED in second voting interval" in {
    val checkHeight: Int = votingInterval * 2
    val activationStatusInfo = activationStatus(nodes.last, checkHeight, featureNum, waitCompletion)

    assertApprovedStatus(activationStatusInfo, votingInterval * 3, NodeFeatureStatus.Voted)
  }

  "blockchain status is ACTIVATED in third voting interval" in {
    val checkHeight: Int = votingInterval * 3
    val activationStatusInfo = activationStatus(nodes.last, checkHeight, featureNum, waitCompletion)

    assertActivatedStatus(activationStatusInfo, checkHeight, NodeFeatureStatus.Voted)
  }


  object ActivationFeatureTestSuite {

    val votingInterval = 20
    val blocksForActivation = 20 // should be even
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
         |}""".stripMargin
    )


    val NodesCount: Int = 4

    val Configs: Seq[Config] = Random.shuffle(NodeConfigs.Default).take(NodesCount).map(supportedNodes.withFallback(_))

  }

}
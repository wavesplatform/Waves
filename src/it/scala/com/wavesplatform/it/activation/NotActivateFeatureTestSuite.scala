package com.wavesplatform.it.activation

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.features.BlockchainFeatureStatus
import com.wavesplatform.features.api.{ActivationStatusFeature, NodeFeatureStatus}
import com.wavesplatform.it.{Docker, Node, NodeConfigs, ReportingTestName}
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.Await
import scala.util.Random

class NotActivateFeatureTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with CancelAfterFailure
  with ActivationStatusRequest with ReportingTestName {

  import NotActivateFeatureTestSuite._

  private lazy val docker = Docker(getClass)
  override lazy val nodes: Seq[Node] = docker.startNodes(Configs)
  private var activationStatusInfoBefore = Option.empty[ActivationStatusFeature]
  private var activationStatusInfoAfter = Option.empty[ActivationStatusFeature]

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    log.debug(s"There are ${nodes.size} in tests") // Initializing of a lazy variable
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    docker.close()
  }

  "get activation status info" in {
    activationStatusInfoBefore = Some(activationStatus(nodes, votingInterval - 1, votingFeatureNum, 4.minute))
    activationStatusInfoAfter = Some(activationStatus(nodes, votingInterval + 1, votingFeatureNum, 4.minute))
  }

  "supported blocks is not increased when nobody votes for feature" in {
    val generatedBlocks = Await.result(nodes.head.blockSeq(1, votingInterval - 1), 2.minute)
    val featuresMapInGeneratedBlocks = generatedBlocks.flatMap(b => b.features.getOrElse(Seq.empty)).groupBy(x => x)
    val votesForFeature1 = featuresMapInGeneratedBlocks.getOrElse(votingFeatureNum, Seq.empty).length

    votesForFeature1 shouldBe 0
    activationStatusInfoBefore.foreach(assertVotingStatus(_, votesForFeature1, BlockchainFeatureStatus.Undefined, NodeFeatureStatus.Implemented))
  }

  "feature is still in VOTING status on the next voting interval" in {
    activationStatusInfoAfter.foreach(assertVotingStatus(_, 0, BlockchainFeatureStatus.Undefined, NodeFeatureStatus.Implemented))
  }

  object NotActivateFeatureTestSuite {

    val NodesCount: Int = 4

    val votingInterval = 14
    val blocksForActivation = 14
    val nonVotingFeatureNum: Short = 2

    private val nonSupportedNodes = ConfigFactory.parseString(
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
         |  features.supported=[$nonVotingFeatureNum]
         |  miner.quorum = ${NodesCount - 1}
         |}""".stripMargin
    )

    val votingFeatureNum: Short = 1

    val Configs: Seq[Config] = Random.shuffle(NodeConfigs.Default.init).take(NodesCount).map(nonSupportedNodes.withFallback(_))

  }

}
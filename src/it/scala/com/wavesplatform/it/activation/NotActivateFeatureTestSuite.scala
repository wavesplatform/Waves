package com.wavesplatform.it.activation

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.features.BlockchainFeatureStatus
import com.wavesplatform.features.api.NodeFeatureStatus
import com.wavesplatform.it.Docker
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

class NotActivateFeatureTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with CancelAfterFailure with ActivationStatusRequest {

  import NotActivateFeatureTestSuite._

  private val docker = Docker(getClass)
  private val nodes = Configs.map(docker.startNode)
  private val votingFeatureNum: Short = 1
  private val nonVotingFeatureNum: Short = 2

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    Await.result(Future.traverse(nodes)(_.waitForPeers(NodesCount - 1)), 2.minute)
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    docker.close()
  }


  "check that voting starts and supported blocks is not increased when nobody votes for feature" in {

    val checkHeight: Int = votingInterval - 1
    val activationStatusInfo = activationStatus(nodes.head, checkHeight, votingFeatureNum, 2.minute)

    val result = Await.result(nodes.head.blockSeq(1, checkHeight), 2.minute)
    val featuresMap = result.flatMap(b => b.features.getOrElse(Seq.empty)).groupBy(x => x)
    val votesForFeature1 = featuresMap.getOrElse(votingFeatureNum, Seq.empty).length

    votesForFeature1 shouldBe 0

    assertVotingStatus(activationStatusInfo, votesForFeature1,
      BlockchainFeatureStatus.Undefined, NodeFeatureStatus.Implemented)
  }


  "check that feature is still in Voting status on the next interval" in {

    val checkHeight: Int = votingInterval + 1
    val activationStatusInfo = activationStatus(nodes.head, checkHeight, votingFeatureNum, 2.minute)

    assertVotingStatus(activationStatusInfo, 0,
      BlockchainFeatureStatus.Undefined, NodeFeatureStatus.Implemented)
  }


  object NotActivateFeatureTestSuite {

    private val dockerConfigs = Docker.NodeConfigs.getConfigList("nodes").asScala

    val votingInterval = 16
    val blocksForActivation = 16

    private val nonSupportedNodes = ConfigFactory.parseString(
      s"""
         |waves.features{
         |   supported=[$nonVotingFeatureNum]
         |}
         |waves.blockchain.custom.functionality.feature-check-blocks-period = $votingInterval
         |waves.blockchain.custom.functionality.blocks-for-feature-activation = $blocksForActivation
         |waves {
         |   blockchain {
         |     custom {
         |      functionality{
         |       pre-activated-features = {}
         |      }
         |     }
         |   }
         |}
      """.stripMargin
    )

    val NodesCount: Int = 4

    val Configs: Seq[Config] = Random.shuffle(dockerConfigs.init).take(NodesCount).map(nonSupportedNodes.withFallback(_))

  }

}
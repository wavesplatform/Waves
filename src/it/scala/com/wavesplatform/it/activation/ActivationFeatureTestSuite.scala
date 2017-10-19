package com.wavesplatform.it.activation

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.features.BlockchainFeatureStatus
import com.wavesplatform.features.api.{ActivationStatusFeature, NodeFeatureStatus}
import com.wavesplatform.it.{Docker, Node}
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random


class ActivationFeatureTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with CancelAfterFailure with ActivationStatusRequest {

  import ActivationFeatureTestSuite._

  private val docker = Docker(getClass)
  private val nodes: Seq[Node] = Configs.map(docker.startNode)
  private val featureNum: Short = 1

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    Await.result(Future.traverse(nodes)(_.waitForPeers(NodesCount - 1)), 2.minute)
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    docker.close()
  }


  "check that voting starts and supported blocks increased" in {

    val checkHeight: Int = votingInterval * 2 / 3

    val activationStatusWhileVoting = activationStatus(nodes.head, checkHeight, featureNum, 2.minute)

    val result = Await.result(nodes.head.blockSeq(1, checkHeight), 2.minute)
    val map = result.flatMap(b => b.features.getOrElse(Seq.empty)).groupBy(x => x)
    val votesForFeature1 = map.getOrElse(featureNum, Seq.empty).length

    assertVotingStatus(activationStatusWhileVoting, votesForFeature1,
      BlockchainFeatureStatus.Undefined, NodeFeatureStatus.Voted)

    val activationStatusIntervalLastVotingBlock = activationStatus(nodes.head, votingInterval, featureNum, 2.minute)
    assertVotingStatus(activationStatusIntervalLastVotingBlock, blocksForActivation - 1,
      BlockchainFeatureStatus.Undefined, NodeFeatureStatus.Voted)


  }


  "check supported blocks counter resets on the next voting interval" in {
    val checkHeight: Int = votingInterval * 2 - blocksForActivation / 2
    val activationStatusInfo = activationStatus(nodes.last, checkHeight, featureNum, 2.minute)

    activationStatusInfo.supportedBlocks.get shouldBe blocksForActivation / 2
    activationStatusInfo.blockchainStatus shouldBe BlockchainFeatureStatus.Undefined
  }

  "check APPROVED blockchain status in second voting interval" in {

    val checkHeight: Int = votingInterval * 2
    val activationStatusInfo = activationStatus(nodes.last, checkHeight, featureNum, 5.minute)

    assertVotingStatus(activationStatusInfo, votingInterval * 3,
      BlockchainFeatureStatus.Approved, NodeFeatureStatus.Voted)
  }

  "check ACTIVATED status in second voting interval" in {
    val checkHeight: Int = votingInterval * 3
    val activationStatusInfo = activationStatus(nodes.last, checkHeight, featureNum, 2.minute)

    assertVotingStatus(activationStatusInfo, checkHeight,
      BlockchainFeatureStatus.Activated, NodeFeatureStatus.Voted)
  }


  object ActivationFeatureTestSuite {

    private val dockerConfigs = Docker.NodeConfigs.getConfigList("nodes").asScala

    val votingInterval = 15
    val blocksForActivation = 15

    private val supportedNodes = ConfigFactory.parseString(
      s"""
         |waves.features{
         |   supported=[$featureNum]
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

    val Configs: Seq[Config] = Random.shuffle(dockerConfigs.init).take(NodesCount).map(supportedNodes.withFallback(_))

  }

}
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

class ActivationFeatureTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with CancelAfterFailure {

  import ActivationFeatureTestSuite._

  private val docker = Docker(getClass)
  private val nodes = Configs.map(docker.startNode)

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    Await.result(Future.traverse(nodes)(_.waitForPeers(NodesCount - 1)), 2.minute)
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    docker.close()
  }


  "check that voting starts and supported blocks increased" in {

    val checkHeight: Int = checkPeriod * 2 / 3
    Await.result(nodes.head.waitForHeight(checkHeight), 2.minute)

    val result = Await.result(nodes.head.blockSeq(1, checkHeight), 2.minute)

    val map = result.flatMap(b => b.features.getOrElse(Seq.empty)).groupBy(x => x)
    val votesForFeature1 = map.getOrElse(1, Seq.empty).length

    val activationStatusForNonSupportedNode = Await.result(nodes.head.activationStatus, 2.minute)
    val activationFeatureNonSupportedNodeInfo = activationStatusForNonSupportedNode.features.find(_.id == 1).get
    activationFeatureNonSupportedNodeInfo.supportedBlocks.get shouldBe votesForFeature1
    activationFeatureNonSupportedNodeInfo.blockchainStatus shouldBe BlockchainFeatureStatus.Undefined
    activationFeatureNonSupportedNodeInfo.nodeStatus shouldBe NodeFeatureStatus.Voted

    val activationStatusForSupportedNode = Await.result(nodes.last.activationStatus, 2.minute)
    val activationFeatureSupportedNodeInfo = activationStatusForSupportedNode.features.find(_.id == 1).get
    activationFeatureSupportedNodeInfo.supportedBlocks.get shouldBe votesForFeature1
    activationFeatureSupportedNodeInfo.blockchainStatus shouldBe BlockchainFeatureStatus.Undefined
    activationFeatureSupportedNodeInfo.nodeStatus shouldBe NodeFeatureStatus.Voted
  }

  "check supported blocks counter resets on the next voting interval" in{
    val checkHeight: Int = checkPeriod * 2 - blocksForActivation

    val result = Await.result(nodes.head.blockSeq(checkPeriod + 1, checkHeight), 2.minute)

    val map = result.flatMap(b => b.features.getOrElse(Seq.empty)).groupBy(x => x)
    val votesForFeature1 = map.getOrElse(1, Seq.empty).length

    Await.result(nodes.head.waitForHeight(checkHeight), 5.minute)
    val activationStatusForSupportedNode = Await.result(nodes.last.activationStatus, 5.minute)
    val activationFeatureSupportedNodeInfo = activationStatusForSupportedNode.features.find(_.id == 1).get
    activationFeatureSupportedNodeInfo.supportedBlocks.get shouldBe votesForFeature1
    activationFeatureSupportedNodeInfo.blockchainStatus shouldBe BlockchainFeatureStatus.Undefined

  }

  "check APPROVED status in second voting interval" in {

    val checkHeight: Int = checkPeriod * 2 -2


    Await.result(nodes.head.waitForHeight(checkHeight), 5.minute)
    val activationStatusForSupportedNode = Await.result(nodes.last.activationStatus, 5.minute)
    val activationFeatureSupportedNodeInfo = activationStatusForSupportedNode.features.find(_.id == 1).get

    activationFeatureSupportedNodeInfo.blockchainStatus shouldBe BlockchainFeatureStatus.Approved
    activationFeatureSupportedNodeInfo.activationHeight.get shouldBe checkPeriod*2

  }

  "check VOTED status in second voting interval" in {
    val checkHeight: Int = checkPeriod * 2
    Await.result(nodes.head.waitForHeight(checkHeight), 5.minute)

    val result = Await.result(nodes.head.blockSeq(1, checkHeight), 5.minute)

    val map = result.flatMap(b => b.features.getOrElse(Seq.empty)).groupBy(x => x)
    val votesForFeature1 = map.getOrElse(1, Seq.empty).length
    val activationStatusForNonSupportedNode = Await.result(nodes.head.activationStatus, 5.minute)
    val activationFeatureNonSupportedNodeInfo = activationStatusForNonSupportedNode.features.find(_.id == 1).get

    activationFeatureNonSupportedNodeInfo.activationHeight.get shouldBe checkHeight*2
    activationFeatureNonSupportedNodeInfo.blockchainStatus shouldBe BlockchainFeatureStatus.Activated
    activationFeatureNonSupportedNodeInfo.nodeStatus shouldBe NodeFeatureStatus.Implemented

    val activationStatusForSupportedNode = Await.result(nodes.last.activationStatus, 5.minute)
    val activationFeatureSupportedNodeInfo = activationStatusForSupportedNode.features.find(_.id == 1).get

    activationFeatureSupportedNodeInfo.supportedBlocks.get shouldBe votesForFeature1
    activationFeatureSupportedNodeInfo.blockchainStatus shouldBe BlockchainFeatureStatus.Activated
    activationFeatureSupportedNodeInfo.nodeStatus shouldBe NodeFeatureStatus.Implemented


  }


  object ActivationFeatureTestSuite {

    private val dockerConfigs = Docker.NodeConfigs.getConfigList("nodes").asScala

    val checkPeriod = 30
    val blocksForActivation = 25

    private val supportedNodes = ConfigFactory.parseString(
      s"""
         |waves.features{
         |   supported=[1]
         |}
         |waves.blockchain.custom.functionality.feature-check-blocks-period = $checkPeriod
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
    private val nonSupportedNodes = ConfigFactory.parseString(
      s"""
         |waves.features{
         | supported=[1]
         |}
         |waves.blockchain.custom.functionality.feature-check-blocks-period = $checkPeriod
         |waves.blockchain.custom.functionality.blocks-for-feature-activation = $blocksForActivation
         |
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

    val Configs: Seq[Config] = Seq(nonSupportedNodes.withFallback(dockerConfigs.last)) ++
      Random.shuffle(dockerConfigs.init).take(NodesCount - 1).map(supportedNodes.withFallback(_))

  }

}
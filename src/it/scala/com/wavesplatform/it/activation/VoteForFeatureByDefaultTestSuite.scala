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

class VoteForFeatureByDefaultTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with CancelAfterFailure with ActivationStatusRequest {

  import VoteForFeatureByDefaultTestSuite._

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

    val checkHeight: Int = votingInterval * 2 / 3
    val supportedNodeActivationInfo = activationStatus(nodes.last, checkHeight, defaultVotingFeatureNum, 2.minute)

    val result = Await.result(nodes.head.blockSeq(1, checkHeight), 2.minute)
    val featuresMap = result.flatMap(b => b.features.getOrElse(Seq.empty)).groupBy(x => x)
    val votesForFeature1 = featuresMap.getOrElse(defaultVotingFeatureNum, Seq.empty).length

    assertVotingStatus(supportedNodeActivationInfo, votesForFeature1, BlockchainFeatureStatus.Undefined, NodeFeatureStatus.Voted)

    val nonSupportedNodeActivationInfo = activationStatus(nodes.head, checkHeight, defaultVotingFeatureNum, 2.minute)
    assertVotingStatus(nonSupportedNodeActivationInfo, votesForFeature1, BlockchainFeatureStatus.Undefined, NodeFeatureStatus.Implemented)

  }

  "check supported blocks counter resets on the next voting interval" in {
    val checkHeight: Int = votingInterval * 2 - blocksForActivation / 2
    val supportedNodeActivationInfo = activationStatus(nodes.last, checkHeight, defaultVotingFeatureNum, 2.minute)

    val result = Await.result(nodes.head.blockSeq(1, checkHeight), 2.minute)
    val featuresMap = result.flatMap(b => b.features.getOrElse(Seq.empty)).groupBy(x => x)
    val votesForFeature1 = featuresMap.getOrElse(defaultVotingFeatureNum, Seq.empty).length

    assertVotingStatus(supportedNodeActivationInfo, votesForFeature1, BlockchainFeatureStatus.Undefined, NodeFeatureStatus.Voted)
  }

  "check APPROVED blockchain status in second voting interval" in {

    val checkHeight: Int = votingInterval * 2

    val supportedNodeActivationInfo = activationStatus(nodes.last, checkHeight, defaultVotingFeatureNum, 2.minute)
    val result = Await.result(nodes.head.blockSeq(1, checkHeight), 2.minute)
    val featuresMap = result.flatMap(b => b.features.getOrElse(Seq.empty)).groupBy(x => x)
    val votesForFeature1 = featuresMap.getOrElse(defaultVotingFeatureNum, Seq.empty).length

    assertApprovedStatus(supportedNodeActivationInfo, votingInterval * 2, NodeFeatureStatus.Voted)
    //
    //    supportedNodeActivationInfo.blockchainStatus shouldBe BlockchainFeatureStatus.Approved
    //    supportedNodeActivationInfo.nodeStatus shouldBe NodeFeatureStatus.Voted
    //    supportedNodeActivationInfo.activationHeight.get shouldBe votingInterval * 3
  }

  "check ACTIVATED status in second voting interval" in {
    val checkHeight: Int = votingInterval * 3

    val supportedNodeActivationInfo = activationStatus(nodes.last, checkHeight, defaultVotingFeatureNum, 2.minute)

    supportedNodeActivationInfo.activationHeight.get shouldBe checkHeight
    supportedNodeActivationInfo.blockchainStatus shouldBe BlockchainFeatureStatus.Activated
    supportedNodeActivationInfo.nodeStatus shouldBe NodeFeatureStatus.Voted
  }


  object VoteForFeatureByDefaultTestSuite {

    private val dockerConfigs = Docker.NodeConfigs.getConfigList("nodes").asScala

    val votingInterval = 18
    val blocksForActivation = 18
    val defaultVotingFeatureNum: Short = 1
    val nonVotingFeatureNum: Short = 2

    val NodesCount: Int = 4


    private val supportedNodes = ConfigFactory.parseString(
      s"""
         |waves.blockchain.custom.functionality.feature-check-blocks-period = $votingInterval
         |waves.blockchain.custom.functionality.blocks-for-feature-activation = $blocksForActivation
         |waves {
         |   blockchain {
         |     custom {
         |        functionality{
         |          pre-activated-features = {}
         |        }
         |
         |        genesis {
         |          average-block-delay: 10000ms
         |          initial-base-target: 200000
         |          timestamp: 1489352400000
         |          block-timestamp: 1489352400000
         |          signature: "2ybcYqV9DB2xNK5zhUmij5Y2geiyDu8fUffnSSmMf2TQasMSHGJrHUQk84ttJ7jV1KQ6S8dT8WGf125WRzomhzj5"
         |          initial-balance: 10000000000000000
         |          transactions = [
         |            {recipient: "3FSXH1sG9Rx5pMkHdmMppTmtAGSCABBpYuV", amount: 250000000000000},
         |            {recipient: "3FgbbKNWQEdcEoMPspDwLUD8sGQbP5SxPPo", amount: 250000000000000},
         |            {recipient: "3FXKqpGC3WKzBrjdVR7zmJS3wY1kfeHLkk9", amount: 250000000000000},
         |            {recipient: "3FfgMjbebfjckwCWHU5AXdwZd2uuEh5VsZS", amount: 250000000000000}
         |          ]
         |        }
         |
         |      }
         |   }
         |}
      """.stripMargin
    )
    private val nonSupportedNodes = ConfigFactory.parseString(
      s"""
         |waves.features{
         | supported=[$nonVotingFeatureNum]
         |}
         |waves.blockchain.custom.functionality.feature-check-blocks-period = $votingInterval
         |waves.blockchain.custom.functionality.blocks-for-feature-activation = $blocksForActivation
         |
         |waves {
         |   blockchain {
         |     custom {
         |        functionality{
         |          pre-activated-features = {}
         |        }
         |        genesis {
         |          average-block-delay: 10000ms
         |          initial-base-target: 200000
         |          timestamp: 1489352400000
         |          block-timestamp: 1489352400000
         |          signature: "2ybcYqV9DB2xNK5zhUmij5Y2geiyDu8fUffnSSmMf2TQasMSHGJrHUQk84ttJ7jV1KQ6S8dT8WGf125WRzomhzj5"
         |          initial-balance: 10000000000000000
         |          transactions = [
         |            {recipient: "3FSXH1sG9Rx5pMkHdmMppTmtAGSCABBpYuV", amount: 250000000000000},
         |            {recipient: "3FgbbKNWQEdcEoMPspDwLUD8sGQbP5SxPPo", amount: 250000000000000},
         |            {recipient: "3FXKqpGC3WKzBrjdVR7zmJS3wY1kfeHLkk9", amount: 250000000000000},
         |            {recipient: "3FfgMjbebfjckwCWHU5AXdwZd2uuEh5VsZS", amount: 250000000000000}
         |          ]
         |        }
         |

         |      }
         |   }
         |}
      """.stripMargin

    )


    val Configs: Seq[Config] = Seq(nonSupportedNodes.withFallback(dockerConfigs(1))) ++
      Seq(supportedNodes.withFallback(dockerConfigs(0))) ++
      Seq(supportedNodes.withFallback(dockerConfigs(2))) ++
      Seq(supportedNodes.withFallback(dockerConfigs(3)))

  }

}
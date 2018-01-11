package com.wavesplatform.it
package activation

import com.typesafe.config.Config
import com.wavesplatform.features.BlockchainFeatureStatus
import com.wavesplatform.features.api.NodeFeatureStatus
import org.scalatest.{CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class VoteForFeatureByDefaultTestSuite extends FreeSpec with Matchers with CancelAfterFailure
  with ActivationStatusRequest with ReportingTestName {

  private val votingInterval = 25
  private val blocksForActivation = 18
  private val defaultVotingFeatureNum: Short = 1
  private val nonVotingFeatureNum: Short = 2

  override protected def nodeConfigs: Seq[Config] = NodeConfigs.newBuilder
    .overrideBase(_.raw(
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
         |  waves.features.supported=[$defaultVotingFeatureNum]
         |  miner.quorum = 3
         |}""".stripMargin
    ))
    .withDefault(3)
    .withSpecial(_.raw(s"waves.features.supported=[$nonVotingFeatureNum]"))
    .buildNonConflicting()

  private def supportedNodes = nodes.init
  private def notSupportedNode = nodes.last

  "supported blocks increased when voting starts, one node votes against, three by default" in {
    val checkHeight: Int = votingInterval * 2 / 3

    val supportedNodeActivationInfo = activationStatus(supportedNodes, checkHeight, defaultVotingFeatureNum, 4.minute)
    val nonSupportedNodeActivationInfo = Await
      .result(notSupportedNode.activationStatus, 1.minute)
      .features
      .find(_.id == defaultVotingFeatureNum)
      .get

    val generatedBlocks = Await.result(nodes.last.blockSeq(1, checkHeight), 2.minute)
    val featuresMapInGeneratedBlocks = generatedBlocks.flatMap(b => b.features.getOrElse(Seq.empty)).groupBy(x => x)
    val votesForFeature1 = featuresMapInGeneratedBlocks.getOrElse(defaultVotingFeatureNum, Seq.empty).length

    assertVotingStatus(supportedNodeActivationInfo, votesForFeature1, BlockchainFeatureStatus.Undefined, NodeFeatureStatus.Voted)
    nonSupportedNodeActivationInfo.nodeStatus shouldBe NodeFeatureStatus.Implemented
  }

  "blockchain status is APPROVED in second voting interval, one node votes against, three by default" in {
    val checkHeight: Int = votingInterval * 2 - blocksForActivation / 2

    val supportedNodeActivationInfo = activationStatus(supportedNodes, checkHeight, defaultVotingFeatureNum, 3.minute)
    assertApprovedStatus(supportedNodeActivationInfo, votingInterval * 2, NodeFeatureStatus.Voted)
  }

  "blockchain status is ACTIVATED in the end of second voting interval, one node votes against, three by default" in {
    val checkHeight: Int = votingInterval * 2

    val supportedNodeActivationInfo = activationStatus(supportedNodes, checkHeight, defaultVotingFeatureNum, 2.minute)
    assertActivatedStatus(supportedNodeActivationInfo, checkHeight, NodeFeatureStatus.Voted)
  }

}

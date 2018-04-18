package com.wavesplatform.it.async.activation

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory.{parseString => cfg}
import com.wavesplatform.features.BlockchainFeatureStatus
import com.wavesplatform.features.api.NodeFeatureStatus
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.{NodeConfigs, ReportingTestName}
import org.scalatest.{CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class VoteForFeatureByDefaultTestSuite
    extends FreeSpec
    with Matchers
    with CancelAfterFailure
    with NodesFromDocker
    with ActivationStatusRequest
    with ReportingTestName {

  import VoteForFeatureByDefaultTestSuite._

  override protected def nodeConfigs: Seq[Config] =
    (cfg(s"waves.features.supported=[$nonVotingFeatureNum]") +: Seq.fill(3)(cfg(s"waves.features.supported=[$defaultVotingFeatureNum]")))
      .zip(Seq(NodeConfigs.Default.head, NodeConfigs.Default(6), NodeConfigs.Default(7), NodeConfigs.Default(8)))
      .map { case (o, t) => o.withFallback(sharedConfig).withFallback(t) }

  private def supportingNodes   = nodes.tail
  private def notSupportingNode = nodes.head

  "supported blocks increased when voting starts, one node votes against, three by default" in {
    val checkHeight: Int = votingInterval * 2 / 3

    val supportingNodeActivationInfo = activationStatus(supportingNodes, checkHeight, defaultVotingFeatureNum, 4.minute)
    val nonSupportingNodeActivationInfo = Await
      .result(notSupportingNode.activationStatus, 1.minute)
      .features
      .find(_.id == defaultVotingFeatureNum)
      .get

    val generatedBlocks              = Await.result(nodes.last.blockSeq(1, checkHeight), 2.minute)
    val featuresMapInGeneratedBlocks = generatedBlocks.flatMap(b => b.features.getOrElse(Seq.empty)).groupBy(identity).mapValues(_.length)
    val votesForFeature1             = featuresMapInGeneratedBlocks.getOrElse(defaultVotingFeatureNum, 0)

    assertVotingStatus(supportingNodeActivationInfo, votesForFeature1, BlockchainFeatureStatus.Undefined, NodeFeatureStatus.Voted)
    nonSupportingNodeActivationInfo.nodeStatus shouldBe NodeFeatureStatus.Implemented
  }

  "blockchain status is APPROVED in second voting interval, one node votes against, three by default" in {
    val checkHeight: Int             = votingInterval * 2 - blocksForActivation / 2
    val supportingNodeActivationInfo = activationStatus(supportingNodes, checkHeight, defaultVotingFeatureNum, 3.minute)
    assertApprovedStatus(supportingNodeActivationInfo, votingInterval * 2, NodeFeatureStatus.Voted)
  }

  "blockchain status is ACTIVATED in the end of second voting interval, one node votes against, three by default" in {
    val checkHeight: Int = votingInterval * 2

    val supportedNodeActivationInfo = activationStatus(supportingNodes, checkHeight, defaultVotingFeatureNum, 2.minute)
    assertActivatedStatus(supportedNodeActivationInfo, checkHeight, NodeFeatureStatus.Voted)
  }

}

object VoteForFeatureByDefaultTestSuite {
  private val defaultVotingFeatureNum: Short = 1
  private val nonVotingFeatureNum: Short     = 2
  private val votingInterval                 = 25
  private val blocksForActivation            = 18

  private val sharedConfig = cfg(s"""waves {
      |  blockchain.custom.functionality {
      |    pre-activated-features = null
      |    feature-check-blocks-period = $votingInterval
      |    blocks-for-feature-activation = $blocksForActivation
      |  }
      |  miner.quorum = 3
      |}""".stripMargin)
}

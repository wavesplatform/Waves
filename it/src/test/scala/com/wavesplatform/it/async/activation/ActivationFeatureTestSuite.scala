package com.wavesplatform.it
package activation

import com.typesafe.config.Config
import com.wavesplatform.features.BlockchainFeatureStatus
import com.wavesplatform.features.api.NodeFeatureStatus
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.transactions.NodesFromDocker
import org.scalatest.{CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class ActivationFeatureTestSuite extends FreeSpec with Matchers with CancelAfterFailure with NodesFromDocker
  with ActivationStatusRequest with ReportingTestName {

  private val waitCompletion = 6.minutes

  private val votingInterval = 12
  private val blocksForActivation = 12 // should be even
  private val featureNum: Short = 1

  override protected def nodeConfigs: Seq[Config] = NodeConfigs.newBuilder
    .overrideBase(_.raw(
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
         |  miner.quorum = 3
         |}""".stripMargin
    ))
    .withDefault(4)
    .buildNonConflicting()

  "supported blocks increased when voting starts" in {
    val checkHeight: Int = votingInterval * 2 / 3

    val activationStatusWhileVoting = activationStatus(nodes, checkHeight, featureNum, waitCompletion)
    val activationStatusIntervalLastVotingBlock = activationStatus(nodes, votingInterval, featureNum, waitCompletion)

    val generatedBlocks = Await.result(nodes.head.blockSeq(1, checkHeight), waitCompletion)
    val featuresMapInGeneratedBlocks = generatedBlocks.flatMap(b => b.features.getOrElse(Seq.empty)).groupBy(x => x)
    val votesForFeature1 = featuresMapInGeneratedBlocks.getOrElse(featureNum, Seq.empty).length

    assertVotingStatus(activationStatusWhileVoting, votesForFeature1, BlockchainFeatureStatus.Undefined, NodeFeatureStatus.Voted)
    assertVotingStatus(activationStatusIntervalLastVotingBlock, blocksForActivation - 1, BlockchainFeatureStatus.Undefined, NodeFeatureStatus.Voted)
  }

  "supported blocks counter resets on the next voting interval" in {
    val checkHeight: Int = votingInterval * 2 - blocksForActivation / 2
    val info = activationStatus(nodes, checkHeight, featureNum, waitCompletion)
    info.supportingBlocks.get shouldBe blocksForActivation / 2
    info.blockchainStatus shouldBe BlockchainFeatureStatus.Undefined
  }

  "blockchain status is APPROVED in second voting interval" in {
    val checkHeight: Int = votingInterval * 2
    val info = activationStatus(nodes, checkHeight, featureNum, waitCompletion)

    // Activation will be on a next voting interval
    assertApprovedStatus(info, checkHeight + votingInterval, NodeFeatureStatus.Voted)
  }

  "blockchain status is ACTIVATED in third voting interval" in {
    val checkHeight: Int = votingInterval * 3
    val info = activationStatus(nodes, checkHeight, featureNum, waitCompletion)
    assertActivatedStatus(info, checkHeight, NodeFeatureStatus.Voted)
  }

}
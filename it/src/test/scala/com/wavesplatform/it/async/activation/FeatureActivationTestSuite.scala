package com.wavesplatform.it.async.activation

import com.typesafe.config.Config
import com.wavesplatform.features.api.NodeFeatureStatus
import com.wavesplatform.features.{BlockchainFeatureStatus, BlockchainFeatures}
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.{NodeConfigs, ReportingTestName}
import org.scalatest.{CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class FeatureActivationTestSuite
    extends FreeSpec
    with Matchers
    with CancelAfterFailure
    with NodesFromDocker
    with ActivationStatusRequest
    with ReportingTestName {

  private val waitCompletion = 6.minutes

  private val votingInterval      = 12
  private val blocksForActivation = 12 // should be even
  private val featureNum: Short   = BlockchainFeatures.SmallerMinimalGeneratingBalance.id
  private val featureDescr        = BlockchainFeatures.SmallerMinimalGeneratingBalance.description

  override protected def nodeConfigs: Seq[Config] = {
    NodeConfigs.newBuilder
      .overrideBase(_.raw(s"""waves {
                               |  blockchain.custom.functionality {
                               |    pre-activated-features = null
                               |    feature-check-blocks-period = $votingInterval
                               |    blocks-for-feature-activation = $blocksForActivation
                               |  }
                               |  features.supported = [$featureNum]
                               |  miner.quorum = 3
                               |}""".stripMargin))
      .withDefault(4)
      .buildNonConflicting()
  }

  "supported blocks increased when voting starts" in {
    val checkHeight = votingInterval * 2 / 3
    val status      = activationStatus(nodes, checkHeight, featureNum, waitCompletion)
    status.description shouldBe featureDescr
    assertVotingStatus(status, status.supportingBlocks.get, BlockchainFeatureStatus.Undefined, NodeFeatureStatus.Voted)
  }

  "supported blocks counter resets on the next voting interval" in {
    val checkHeight = votingInterval * 2 - blocksForActivation / 2
    val info        = activationStatus(nodes, checkHeight, featureNum, waitCompletion)
    info.blockchainStatus shouldBe BlockchainFeatureStatus.Undefined
  }

  "blockchain status is APPROVED in second voting interval" in {
    val checkHeight = votingInterval * 2
    val statusInfo  = activationStatus(nodes, checkHeight, featureNum, waitCompletion)
    statusInfo.description shouldBe featureDescr
    // Activation will be on a next voting interval
    assertApprovedStatus(statusInfo, checkHeight + votingInterval, NodeFeatureStatus.Voted)
  }

  "blockchain status is ACTIVATED in third voting interval" in {
    val checkHeight = votingInterval * 3
    val statusInfo  = activationStatus(nodes, checkHeight, featureNum, waitCompletion)
    statusInfo.description shouldBe featureDescr
    assertActivatedStatus(statusInfo, checkHeight, NodeFeatureStatus.Voted)
  }
}

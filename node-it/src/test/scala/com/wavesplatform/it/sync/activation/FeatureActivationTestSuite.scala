package com.wavesplatform.it.sync.activation

import com.typesafe.config.Config
import com.wavesplatform.features.api.NodeFeatureStatus
import com.wavesplatform.features.{BlockchainFeatureStatus, BlockchainFeatures}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.{BaseFreeSpec, NodeConfigs, ReportingTestName}

class FeatureActivationTestSuite extends BaseFreeSpec with ActivationStatusRequest with ReportingTestName {

  private val votingInterval      = 12
  private val blocksForActivation = 12 // should be even
  private val featureNum: Short   = BlockchainFeatures.SmallerMinimalGeneratingBalance.id
  private val featureDescr        = BlockchainFeatures.SmallerMinimalGeneratingBalance.description

  override protected def nodeConfigs: Seq[Config] = {
    NodeConfigs.newBuilder
      .overrideBase(_.raw(s"""waves {
                             |  blockchain.custom.functionality {
                             |    pre-activated-features = {}
                             |    feature-check-blocks-period = $votingInterval
                             |    blocks-for-feature-activation = $blocksForActivation
                             |  }
                             |  features.supported = [$featureNum]
                             |  miner.quorum = 1
                             |}""".stripMargin))
      .withDefault(2)
      .buildNonConflicting()
  }

  "supported blocks increased when voting starts" in {
    nodes.waitForHeight(votingInterval * 2 / 3)
    val status = nodes.map(_.featureActivationStatus(featureNum))
    status.foreach { s =>
      s.description shouldBe featureDescr
      assertVotingStatus(s, s.supportingBlocks.get, BlockchainFeatureStatus.Undefined, NodeFeatureStatus.Voted)
    }
  }

  "supported blocks counter resets on the next voting interval" in {
    nodes.waitForHeight(votingInterval * 2 - blocksForActivation / 2)
    val info = nodes.map(_.featureActivationStatus(featureNum))
    info.foreach(i => i.blockchainStatus shouldBe BlockchainFeatureStatus.Undefined)
  }

  "blockchain status is APPROVED in second voting interval" in {
    val checkHeight = votingInterval * 2
    nodes.waitForHeight(checkHeight)
    val statusInfo = nodes.map(_.featureActivationStatus(featureNum))
    statusInfo.foreach { si =>
      si.description shouldBe featureDescr
      // Activation will be on a next voting interval
      assertApprovedStatus(si, checkHeight + votingInterval, NodeFeatureStatus.Voted)
    }
  }

  "blockchain status is ACTIVATED in third voting interval" in {
    val checkHeight = votingInterval * 3
    nodes.waitForHeight(checkHeight)
    val statusInfo = nodes.map(_.featureActivationStatus(featureNum))
    statusInfo.foreach { si =>
      si.description shouldBe featureDescr
      assertActivatedStatus(si, checkHeight, NodeFeatureStatus.Implemented)
    }
  }
}

package com.wavesplatform.it.sync.activation

import com.wavesplatform.features.BlockchainFeatureStatus
import com.wavesplatform.features.api.{FeatureActivationStatus, NodeFeatureStatus}
import org.scalactic.source.Position
import org.scalatest.Matchers

trait ActivationStatusRequest extends Matchers {
  def assertVotingStatus(fas: FeatureActivationStatus,
                         supportedBlocks: Int,
                         blockchainFeatureStatus: BlockchainFeatureStatus,
                         nodeFeatureStatus: NodeFeatureStatus)(implicit pos: Position): Unit = {
    withClue("supportedBlocks") {
      fas.supportingBlocks should contain(supportedBlocks)
    }
    withClue("blockchainStatus") {
      fas.blockchainStatus shouldBe blockchainFeatureStatus
    }
    withClue("nodeStatus") {
      fas.nodeStatus shouldBe nodeFeatureStatus
    }
  }

  def assertApprovedStatus(fas: FeatureActivationStatus, height: Int, nodeFeatureStatus: NodeFeatureStatus)(implicit pos: Position): Unit = {
    withClue("activationHeight") {
      fas.activationHeight should contain(height)
    }
    withClue("blockchainStatus") {
      fas.blockchainStatus shouldBe BlockchainFeatureStatus.Approved
    }
    withClue("nodeStatus") {
      fas.nodeStatus shouldBe nodeFeatureStatus
    }
  }

  def assertActivatedStatus(fas: FeatureActivationStatus, height: Int, nodeFeatureStatus: NodeFeatureStatus)(implicit pos: Position): Unit = {
    withClue("activationHeight") {
      fas.activationHeight should contain(height)
    }
    withClue("blockchainStatus") {
      fas.blockchainStatus shouldBe BlockchainFeatureStatus.Activated
    }
    withClue("nodeStatus") {
      fas.nodeStatus shouldBe nodeFeatureStatus
    }
  }
}

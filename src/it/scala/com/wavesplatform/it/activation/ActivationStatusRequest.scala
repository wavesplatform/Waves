package com.wavesplatform.it.activation

import com.wavesplatform.features.BlockchainFeatureStatus
import com.wavesplatform.features.api.{ActivationStatus, ActivationStatusFeature, NodeFeatureStatus}
import com.wavesplatform.it.Node
import org.scalatest.Matchers

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}

trait ActivationStatusRequest extends Matchers {
  def activationStatus(node: Node, height: Int, featureNum: Short, timeout: Duration)
                      (implicit ec: ExecutionContext): ActivationStatusFeature = Await.result(
    node
      .waitFor[ActivationStatus](_.activationStatus, _.height >= height, 1.second)
      .map { r =>
        if (r.height > height) throw new IllegalStateException(s"Height (${r.height}) is more than expected")
        r
      }
      .map(_.features.find(_.id == featureNum).get),
    timeout
  )

  def assertVotingStatus(activationStatusFeature: ActivationStatusFeature, supportedBlocks: Int, blockchainFeatureStatus: BlockchainFeatureStatus, nodeFeatureStatus: NodeFeatureStatus): Unit = {
    activationStatusFeature.supportedBlocks.get shouldBe supportedBlocks
    activationStatusFeature.blockchainStatus shouldBe blockchainFeatureStatus
    activationStatusFeature.nodeStatus shouldBe nodeFeatureStatus
  }

  def assertApprovedStatus(activationStatusFeature: ActivationStatusFeature, height: Int, nodeFeatureStatus: NodeFeatureStatus): Unit = {
    activationStatusFeature.activationHeight.get shouldBe height
    activationStatusFeature.blockchainStatus shouldBe BlockchainFeatureStatus.Approved
    activationStatusFeature.nodeStatus shouldBe nodeFeatureStatus
  }

  def assertActivatedStatus(activationStatusFeature: ActivationStatusFeature, height: Int, nodeFeatureStatus: NodeFeatureStatus): Unit = {
    activationStatusFeature.activationHeight.get shouldBe height
    activationStatusFeature.blockchainStatus shouldBe BlockchainFeatureStatus.Activated
    activationStatusFeature.nodeStatus shouldBe nodeFeatureStatus
  }
}

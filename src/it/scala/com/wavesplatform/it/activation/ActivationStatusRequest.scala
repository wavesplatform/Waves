package com.wavesplatform.it.activation

import com.wavesplatform.features.BlockchainFeatureStatus
import com.wavesplatform.features.api.{ActivationStatus, ActivationStatusFeature, NodeFeatureStatus}
import com.wavesplatform.it.Node
import org.scalatest.Matchers

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

trait ActivationStatusRequest extends Matchers {
  def activationStatus(node: Node, height: Int, featureNum: Short, timeout: Duration)
                      (implicit ec: ExecutionContext): ActivationStatusFeature = Await.result(
    activationStatusInternal(node, height, featureNum, timeout),
    timeout
  )

  def activationStatus(nodes: Seq[Node], height: Int, featureNum: Short, timeout: Duration)
                      (implicit ec: ExecutionContext): Map[Node, ActivationStatusFeature] = Await.result(
    Future
      .traverse(nodes) { node =>
        activationStatusInternal(node, height, featureNum, timeout).map(node -> _)
      }
      .map(_.toMap),
    timeout
  )

  private def activationStatusInternal(node: Node, height: Int, featureNum: Short, timeout: Duration)
                                      (implicit ec: ExecutionContext): Future[ActivationStatusFeature] = {
    node
      .waitFor[ActivationStatus](_.activationStatus, _.height >= height, 1.second)
      .map { r =>
        if (r.height > height) throw new IllegalStateException(
          s"Height (${r.height}) is more than expected ($height) on ${node.settings.networkSettings.nodeName}"
        )
        r
      }
      .map(_.features.find(_.id == featureNum).get)
  }

  def assertVotingStatus(activationStatusFeature: ActivationStatusFeature,
                         supportedBlocks: Int,
                         blockchainFeatureStatus: BlockchainFeatureStatus,
                         nodeFeatureStatus: NodeFeatureStatus): Unit = {
    withClue("supportedBlocks") {
      activationStatusFeature.supportedBlocks.get shouldBe supportedBlocks
    }
    withClue("blockchainStatus") {
      activationStatusFeature.blockchainStatus shouldBe blockchainFeatureStatus
    }
    withClue("nodeStatus") {
      activationStatusFeature.nodeStatus shouldBe nodeFeatureStatus
    }
  }

  def assertApprovedStatus(activationStatusFeature: ActivationStatusFeature,
                           height: Int,
                           nodeFeatureStatus: NodeFeatureStatus): Unit = {
    withClue("activationHeight") {
      activationStatusFeature.activationHeight.get shouldBe height
    }
    withClue("blockchainStatus") {
      activationStatusFeature.blockchainStatus shouldBe BlockchainFeatureStatus.Approved
    }
    withClue("nodeStatus") {
      activationStatusFeature.nodeStatus shouldBe nodeFeatureStatus
    }
  }

  def assertActivatedStatus(activationStatusFeature: ActivationStatusFeature,
                            height: Int,
                            nodeFeatureStatus: NodeFeatureStatus): Unit = {
    withClue("activationHeight") {
      activationStatusFeature.activationHeight.get shouldBe height
    }
    withClue("blockchainStatus") {
      activationStatusFeature.blockchainStatus shouldBe BlockchainFeatureStatus.Activated
    }
    withClue("nodeStatus") {
      activationStatusFeature.nodeStatus shouldBe nodeFeatureStatus
    }
  }
}

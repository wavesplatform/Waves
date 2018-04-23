package com.wavesplatform.it.async.activation

import com.wavesplatform.features.BlockchainFeatureStatus
import com.wavesplatform.features.api.{ActivationStatus, FeatureActivationStatus, NodeFeatureStatus}
import com.wavesplatform.it.Node
import com.wavesplatform.it.api.AsyncHttpApi._
import org.scalactic.source.Position
import org.scalatest.Matchers

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

trait ActivationStatusRequest extends Matchers {
  def activationStatus(node: Node, height: Int, featureNum: Short, timeout: Duration)(implicit ec: ExecutionContext): FeatureActivationStatus =
    Await.result(
      activationStatusInternal(node, height).map(_.features.find(_.id == featureNum).get),
      timeout
    )

  def activationStatus(nodes: Seq[Node], height: Int, featureNum: Short, timeout: Duration)(implicit ec: ExecutionContext): FeatureActivationStatus =
    Await.result(
      Future
        .traverse(nodes)(activationStatusInternal(_, height).map(_.features.collectFirst { case f if f.id == featureNum => f }))
        .map { xs =>
          xs.head.get
        },
      timeout
    )

  private def activationStatusInternal(node: Node, height: Int): Future[ActivationStatus] = {
    node.waitFor[ActivationStatus](s"activationStatusInternal: height should be >= $height")(_.activationStatus, _.height >= height, 2.second)
  }

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

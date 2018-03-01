package com.wavesplatform.it.activation

import com.wavesplatform.features.BlockchainFeatureStatus
import com.wavesplatform.features.api.{ActivationStatus, ActivationStatusFeature, NodeFeatureStatus}
import com.wavesplatform.it.Node
import com.wavesplatform.it.api.AsyncHttpApi._
import org.scalatest.Matchers

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

trait ActivationStatusRequest extends Matchers {
  def activationStatus(node: Node, height: Int, featureNum: Short, timeout: Duration)
                      (implicit ec: ExecutionContext): ActivationStatusFeature = Await.result(
    activationStatusInternal(node, height).map(_.features.find(_.id == featureNum).get),
    timeout
  )

  def activationStatus(nodes: Seq[Node], height: Int, featureNum: Short, timeout: Duration)
                      (implicit ec: ExecutionContext): ActivationStatusFeature = {
    Await.result(
      Future
        .traverse(nodes)(activationStatusInternal(_, height))
        .map { xs =>
          xs
            .collectFirst {
              case x if x.height == height => x
            }
            .flatMap(_.features.find(_.id == featureNum))
            .getOrElse(throw new NoSuchElementException("Impossible: all nodes is on higher height"))
        },
      timeout
    )
  }

  private def activationStatusInternal(node: Node, height: Int): Future[ActivationStatus] = {
    node.waitFor[ActivationStatus](s"activationStatusInternal: height should be >= $height")(_.activationStatus, _.height >= height, 1.second)
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

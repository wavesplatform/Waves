package com.wavesplatform.api.grpc
import com.google.protobuf.ByteString
import com.google.protobuf.empty.Empty
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.features.{BlockchainFeatureStatus, BlockchainFeatures}
import com.wavesplatform.settings.FeaturesSettings
import com.wavesplatform.state.Blockchain
import monix.execution.Scheduler

import scala.concurrent.Future

class BlockchainApiGrpcImpl(blockchain: Blockchain, featuresSettings: FeaturesSettings)(implicit sc: Scheduler)
    extends BlockchainApiGrpc.BlockchainApi {

  override def getActivationStatus(request: ActivationStatusRequest): Future[ActivationStatusResponse] = Future {
    val functionalitySettings = blockchain.settings.functionalitySettings

    ActivationStatusResponse(
      request.height,
      functionalitySettings.activationWindowSize(request.height),
      functionalitySettings.blocksForFeatureActivation(request.height),
      functionalitySettings.activationWindow(request.height).last,
      (blockchain.featureVotes(request.height).keySet ++
        blockchain.approvedFeatures.keySet ++
        BlockchainFeatures.implemented).toSeq.sorted.map(id => {
        val status = blockchain.featureStatus(id, request.height) match {
          case BlockchainFeatureStatus.Undefined => FeatureActivationStatus.BlockchainFeatureStatus.UNDEFINED
          case BlockchainFeatureStatus.Approved  => FeatureActivationStatus.BlockchainFeatureStatus.APPROVED
          case BlockchainFeatureStatus.Activated => FeatureActivationStatus.BlockchainFeatureStatus.ACTIVATED
        }

        FeatureActivationStatus(
          id,
          BlockchainFeatures.feature(id).fold("Unknown feature")(_.description),
          status,
          (BlockchainFeatures.implemented.contains(id), featuresSettings.supported.contains(id)) match {
            case (false, _) => FeatureActivationStatus.NodeFeatureStatus.NOT_IMPLEMENTED
            case (_, true)  => FeatureActivationStatus.NodeFeatureStatus.VOTED
            case _          => FeatureActivationStatus.NodeFeatureStatus.IMPLEMENTED
          },
          blockchain.featureActivationHeight(id).getOrElse(0),
          if (status.isUndefined) blockchain.featureVotes(request.height).getOrElse(id, 0) else 0
        )
      })
    )
  }

  override def getBaseTarget(request: Empty): Future[BaseTargetResponse] = Future {
    BaseTargetResponse(blockchain.lastBlock.get.consensusData.baseTarget)
  }

  override def getCumulativeScore(request: Empty): Future[ScoreResponse] = Future {
    ScoreResponse(ByteString.copyFrom(blockchain.score.toByteArray))
  }
}

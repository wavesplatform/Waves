package com.wavesplatform.it.sync.activation

import com.typesafe.config.Config
import com.wavesplatform.features.BlockchainFeatureStatus
import com.wavesplatform.features.api.{FeatureActivationStatus, NodeFeatureStatus}
import com.wavesplatform.it.api.BlockHeader
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.{BaseFreeSpec, NodeConfigs}
import com.wavesplatform.state.Height

class NotActivateFeatureTestSuite extends BaseFreeSpec with ActivationStatusRequest {

  private val votingInterval             = 14
  private val blocksForActivation        = 14
  private val votingFeatureNum: Short    = 1
  private val nonVotingFeatureNum: Short = 2

  import NodeConfigs.*
  override protected def nodeConfigs: Seq[Config] =
    Seq(BiggestMiner, Miners(5)).map(
      _.overrides(
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
           |  features.supported=[$nonVotingFeatureNum]
           |  miner.quorum = 1
           |}""".stripMargin
      )
    )

  private var activationStatusInfoBefore = Seq.empty[FeatureActivationStatus]
  private var activationStatusInfoAfter  = Seq.empty[FeatureActivationStatus]

  "get activation status info" in {
    nodes.waitForHeight(Height(votingInterval - 1))
    activationStatusInfoBefore = nodes.map(_.featureActivationStatus(votingFeatureNum))
    nodes.waitForHeight(Height(votingInterval + 1))
    activationStatusInfoAfter = nodes.map(_.featureActivationStatus(votingFeatureNum))
  }

  "supported blocks is not increased when nobody votes for feature" in {
    val generatedBlocks: Seq[BlockHeader] = nodes.head.blockHeadersSeq(Height(1), Height(votingInterval - 1))
    val featuresMapInGeneratedBlocks      = generatedBlocks.flatMap(b => b.features.getOrElse(Seq.empty)).groupBy(x => x)
    val votesForFeature1                  = featuresMapInGeneratedBlocks.getOrElse(votingFeatureNum, Seq.empty).length

    votesForFeature1 shouldBe 0
    activationStatusInfoBefore.foreach(assertVotingStatus(_, votesForFeature1, BlockchainFeatureStatus.Undefined, NodeFeatureStatus.Implemented))
  }

  "feature is still in VOTING status on the next voting interval" in {
    activationStatusInfoAfter.foreach(assertVotingStatus(_, 0, BlockchainFeatureStatus.Undefined, NodeFeatureStatus.Implemented))
  }

}

package com.wavesplatform.features

import com.wavesplatform.block.Block.{NgBlockVersion, PlainBlockVersion, RewardBlockVersion}
import com.wavesplatform.state.Blockchain

object FeatureProvider {
  final implicit class FeatureProviderExt(private val blockchain: Blockchain) extends AnyVal {
    def isFeatureActivated(feature: BlockchainFeature, height: Int = blockchain.height): Boolean =
      blockchain.activatedFeatures.get(feature.id).exists(_ <= height)

    def activatedFeaturesAt(height: Int): Set[Short] =
      blockchain.activatedFeatures.collect {
        case (featureId, activationHeight) if height >= activationHeight => featureId
      }.toSet

    def featureStatus(feature: Short, height: Int): BlockchainFeatureStatus =
      if (blockchain.activatedFeatures.get(feature).exists(_ <= height)) BlockchainFeatureStatus.Activated
      else if (blockchain.approvedFeatures.get(feature).exists(_ <= height)) BlockchainFeatureStatus.Approved
      else BlockchainFeatureStatus.Undefined

    def currentBlockVersion: Byte =
      if (isFeatureActivated(BlockchainFeatures.BlockReward)) RewardBlockVersion
      else if (blockchain.settings.functionalitySettings.blockVersion3AfterHeight < blockchain.height) NgBlockVersion
      else PlainBlockVersion

    def featureActivationHeight(feature: Short): Option[Int] = blockchain.activatedFeatures.get(feature)
    def featureApprovalHeight(feature: Short): Option[Int]   = blockchain.approvedFeatures.get(feature)
  }
}

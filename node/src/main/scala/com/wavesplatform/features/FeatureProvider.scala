package com.wavesplatform.features

import com.wavesplatform.block.Block.{GenesisBlockVersion, NgBlockVersion, PlainBlockVersion, ProtoBlockVersion, RewardBlockVersion}
import com.wavesplatform.lang.v1.traits.domain.Issue
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.assets.IssueTransaction

object FeatureProvider {
  final implicit class FeatureProviderExt(private val blockchain: Blockchain) extends AnyVal {
    def isNFT(issueTransaction: IssueTransaction): Boolean = isNFT(issueTransaction.quantity, issueTransaction.decimals, issueTransaction.reissuable)
    def isNFT(issueAction: Issue): Boolean                 = isNFT(issueAction.quantity, issueAction.decimals, issueAction.isReissuable)
    def isNFT(quantity: Long, decimals: Int, reissuable: Boolean): Boolean =
      isFeatureActivated(BlockchainFeatures.ReduceNFTFee) && quantity == 1 && decimals == 0 && !reissuable

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

    def currentBlockVersion: Byte = blockVersionAt(blockchain.height)
    def nextBlockVersion: Byte    = blockVersionAt(blockchain.height + 1)

    def featureActivationHeight(feature: Short): Option[Int] = blockchain.activatedFeatures.get(feature)
    def featureApprovalHeight(feature: Short): Option[Int]   = blockchain.approvedFeatures.get(feature)

    def blockVersionAt(height: Int): Byte =
      if (isFeatureActivated(BlockchainFeatures.BlockV5, height)) ProtoBlockVersion
      else if (isFeatureActivated(BlockchainFeatures.BlockReward, height)) {
        if (blockchain.activatedFeatures(BlockchainFeatures.BlockReward.id) == height) NgBlockVersion else RewardBlockVersion
      }
      else if (blockchain.settings.functionalitySettings.blockVersion3AfterHeight < height) NgBlockVersion
      else if (height > 1) PlainBlockVersion
      else GenesisBlockVersion
  }
}

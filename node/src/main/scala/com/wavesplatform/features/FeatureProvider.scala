package com.wavesplatform.features

import com.wavesplatform.state.{Blockchain, Height}

object FeatureProvider {
  implicit class FeatureProviderExt(provider: Blockchain) {
    def isFeatureActivated(feature: BlockchainFeature, height: Height = provider.height): Boolean =
      provider.activatedFeatures.get(feature.id).exists(_ <= height)

    def activatedFeaturesAt(height: Height): Set[Short] =
      provider.activatedFeatures.collect {
        case (featureId, activationHeight) if height >= activationHeight => featureId
      }.toSet

    def featureStatus(feature: Short, height: Height): BlockchainFeatureStatus =
      if (provider.activatedFeatures.get(feature).exists(_ <= height)) BlockchainFeatureStatus.Activated
      else if (provider.approvedFeatures.get(feature).exists(_ <= height)) BlockchainFeatureStatus.Approved
      else BlockchainFeatureStatus.Undefined

    def featureActivationHeight(feature: Short): Option[Height] = provider.activatedFeatures.get(feature)

    def featureApprovalHeight(feature: Short): Option[Height] = provider.approvedFeatures.get(feature)
  }
}

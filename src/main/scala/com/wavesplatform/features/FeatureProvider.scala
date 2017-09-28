package com.wavesplatform.features

trait FeatureProvider {
  def featureStatus(feature: Short): BlockchainFeatureStatus
  def featureActivationHeight(feature: Short): Option[Int]
  def isFeatureLocallyActivated(feature: Short): Boolean
}

object FeatureProvider {
  implicit class FeatureProviderExt(provider: FeatureProvider) {
    def activated(feature: BlockchainFeature): Boolean = {



      provider.featureStatus(feature.id) == BlockchainFeatureStatus.Activated
    }
  }
}
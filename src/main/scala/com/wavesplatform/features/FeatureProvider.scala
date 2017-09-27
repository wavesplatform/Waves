package com.wavesplatform.features

trait FeatureProvider {
  def status(feature: Short): FeatureStatus
  def activationHeight(feature: Short): Option[Int]
}

object FeatureProvider {

  implicit class FeatureProviderExt(provider: FeatureProvider) {
    def activated(feature: BlockchainFeature): Boolean = {
      provider.status(feature.id) == FeatureStatus.Activated
    }
  }

}
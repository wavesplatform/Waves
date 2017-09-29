package com.wavesplatform.features

trait FeatureProvider {
  def featureStatus(feature: Short): BlockchainFeatureStatus

  def featureActivationHeight(feature: Short): Option[Int]

  def featureVotesCountWithinActivationWindow(height: Int): Map[Short, Int]

}

object FeatureProvider {

  implicit class FeatureProviderExt(provider: FeatureProvider) {
    def isFeatureActivated(feature: BlockchainFeature): Boolean = {
      provider.featureStatus(feature.id) == BlockchainFeatureStatus.Activated
    }
  }

  def activationWindowOpeningFromHeight(height: Int, activationWindowSize: Int): Int = {
    val r = 1 + height - height % activationWindowSize
    if (r <= height) r else r - activationWindowSize
  }
}

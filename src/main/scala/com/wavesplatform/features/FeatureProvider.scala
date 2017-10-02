package com.wavesplatform.features

trait FeatureProvider {
  def featureStatus(feature: Short, height: Int): BlockchainFeatureStatus

  def activatedFeatures(height: Int): Set[Short]

  def featureActivationHeight(feature: Short): Option[Int]

  def featureVotesCountWithinActivationWindow(height: Int): Map[Short, Int]

}

object FeatureProvider {

  implicit class FeatureProviderExt(provider: FeatureProvider) {
    def isFeatureActivated(feature: BlockchainFeature, height: Int): Boolean = {
      provider.featureStatus(feature.id, height) == BlockchainFeatureStatus.Activated
    }
  }

  def activationWindowOpeningFromHeight(height: Int, activationWindowSize: Int): Int = {
    val r = 1 + height - height % activationWindowSize
    if (r <= height) r else r - activationWindowSize
  }
}

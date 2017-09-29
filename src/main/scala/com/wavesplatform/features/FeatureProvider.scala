package com.wavesplatform.features

trait FeatureProvider {
  val ActivationWindowSize: Int
  val MinVotesWithinWindowToActivateFeature: Int

  def featureStatus(feature: Short): BlockchainFeatureStatus

  def featureActivationHeight(feature: Short): Option[Int]

  def featureVotesCountWithinActivationWindow(height: Int): Map[Short, Int]

  def activationWindowOpeningFromHeight(height: Int): Int
}

object FeatureProvider {

  implicit class FeatureProviderExt(provider: FeatureProvider) {
    def isFeatureActivated(feature: BlockchainFeature): Boolean = {
      provider.featureStatus(feature.id) == BlockchainFeatureStatus.Activated
    }
  }

}

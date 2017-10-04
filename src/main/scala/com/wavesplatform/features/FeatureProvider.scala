package com.wavesplatform.features

trait FeatureProvider {

  protected val activationWindowSize: Int

  def acceptedFeatures() : Map[Short, Int]

  def featureVotesCountWithinActivationWindow(height: Int): Map[Short, Int]
}

object FeatureProvider {

  implicit class FeatureProviderExt(provider: FeatureProvider) {
    def isFeatureActivated(feature: BlockchainFeature, height: Int): Boolean = {
      provider.featureStatus(feature.id, height) == BlockchainFeatureStatus.Activated
    }

    def featureStatus(feature: Short, height: Int): BlockchainFeatureStatus = {
      featureAcceptedHeight(feature).getOrElse(Int.MaxValue) match {
        case x if x <= height - provider.activationWindowSize => BlockchainFeatureStatus.Activated
        case x if x <= height => BlockchainFeatureStatus.Accepted
        case _ => BlockchainFeatureStatus.Undefined
      }
    }

    def activatedFeatures(height: Int): Set[Short] = provider.acceptedFeatures()
      .filter{case (_, acceptedHeight) => acceptedHeight <= height - provider.activationWindowSize}.keySet

    def featureActivatedHeight(feature: Short): Option[Int] = {
      featureAcceptedHeight(feature).map(h => h + provider.activationWindowSize)
    }

    def featureAcceptedHeight(feature: Short): Option[Int] = provider.acceptedFeatures().get(feature)
  }

  def activationWindowOpeningFromHeight(height: Int, activationWindowSize: Int): Int = {
    val r = 1 + height - height % activationWindowSize
    if (r <= height) r else r - activationWindowSize
  }
}

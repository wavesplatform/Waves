package com.wavesplatform.features

trait FeatureProvider {

  protected val activationWindowSize: Int

  def approvedFeatures() : Map[Short, Int]

  def featureVotesCountWithinActivationWindow(height: Int): Map[Short, Int]
}

object FeatureProvider {

  implicit class FeatureProviderExt(provider: FeatureProvider) {
    def isFeatureActivated(feature: BlockchainFeature, height: Int): Boolean = {
      provider.featureStatus(feature.id, height) == BlockchainFeatureStatus.Activated
    }

    def featureStatus(feature: Short, height: Int): BlockchainFeatureStatus = {
      featureApprovalHeight(feature).getOrElse(Int.MaxValue) match {
        case x if x <= height - provider.activationWindowSize => BlockchainFeatureStatus.Activated
        case x if x <= height => BlockchainFeatureStatus.Approved
        case _ => BlockchainFeatureStatus.Undefined
      }
    }

    def activatedFeatures(height: Int): Set[Short] = provider.approvedFeatures()
      .filter { case (_, acceptedHeight) => acceptedHeight <= height - provider.activationWindowSize }.keySet

    def featureActivationHeight(feature: Short): Option[Int] = {
      featureApprovalHeight(feature).map(h => h + provider.activationWindowSize)
    }

    def featureApprovalHeight(feature: Short): Option[Int] = provider.approvedFeatures().get(feature)
  }

  def votingWindowOpeningFromHeight(height: Int, activationWindowSize: Int): Int =
    ((height - 1) / activationWindowSize) * activationWindowSize + 1
}

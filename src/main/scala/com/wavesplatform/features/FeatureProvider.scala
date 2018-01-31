package com.wavesplatform.features

import com.wavesplatform.settings.FunctionalitySettings

trait FeatureProvider {

  protected def activationWindowSize(height: Int): Int

  def approvedFeatures(): Map[Short, Int]

  def featureVotesCountWithinActivationWindow(height: Int): Map[Short, Int]
}

case class FeaturesProperties(functionalitySettings: FunctionalitySettings) {
  def featureCheckBlocksPeriodAtHeight(height: Int): Int =
    doubleValueAtHeight(height, functionalitySettings.featureCheckBlocksPeriod)

  def blocksForFeatureActivationAtHeight(height: Int): Int =
    doubleValueAtHeight(height, functionalitySettings.blocksForFeatureActivation)

  private def doubleValueAtHeight(height: Int, value: Int): Int =
    if (height > functionalitySettings.doubleFeaturesPeriodsAfterHeight) value * 2 else value
}

object FeatureProvider {

  implicit class FeatureProviderExt(provider: FeatureProvider) {
    def isFeatureActivated(feature: BlockchainFeature, height: Int): Boolean = {
      provider.featureStatus(feature.id, height) == BlockchainFeatureStatus.Activated
    }

    def featureStatus(feature: Short, height: Int): BlockchainFeatureStatus = {
      featureApprovalHeight(feature).getOrElse(Int.MaxValue) match {
        case x if x <= height - provider.activationWindowSize(height) => BlockchainFeatureStatus.Activated
        case x if x <= height => BlockchainFeatureStatus.Approved
        case _ => BlockchainFeatureStatus.Undefined
      }
    }

    def activatedFeatures(height: Int): Set[Short] = provider.approvedFeatures()
      .filter { case (_, acceptedHeight) => acceptedHeight <= height - provider.activationWindowSize(height) }.keySet

    def featureActivationHeight(feature: Short): Option[Int] = {
      featureApprovalHeight(feature).map(h => h + provider.activationWindowSize(h))
    }

    def featureApprovalHeight(feature: Short): Option[Int] = provider.approvedFeatures().get(feature)
  }

  def votingWindowOpeningFromHeight(height: Int, activationWindowSize: Int): Int =
    ((height - 1) / activationWindowSize) * activationWindowSize + 1
}

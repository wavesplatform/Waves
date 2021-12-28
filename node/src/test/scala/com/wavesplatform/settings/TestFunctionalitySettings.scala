package com.wavesplatform.settings

import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}

object TestFunctionalitySettings {
  val Enabled = FunctionalitySettings(featureCheckBlocksPeriod = 10000, blocksForFeatureActivation = 9000, preActivatedFeatures = Map(
      BlockchainFeatures.SmartAccounts.id -> 0,
      BlockchainFeatures.SmartAssets.id   -> 0,
      BlockchainFeatures.FairPoS.id       -> 0,
      BlockchainFeatures.Ride4DApps.id    -> 0
    ), doubleFeaturesPeriodsAfterHeight = Int.MaxValue, estimatorSumOverflowFixHeight = Int.MaxValue)

  def withFeatures(features: BlockchainFeature*): FunctionalitySettings =
    Enabled.copy(preActivatedFeatures = Enabled.preActivatedFeatures ++ features.map(_.id -> 0))

  def withFeaturesByHeight(features: (BlockchainFeature, Int)*): FunctionalitySettings =
    Enabled.copy(preActivatedFeatures = Enabled.preActivatedFeatures ++ features.map { case (f, height) => f.id -> height })

  val Stub: FunctionalitySettings = Enabled.copy(featureCheckBlocksPeriod = 100, blocksForFeatureActivation = 90)

  val EmptyFeaturesSettings: FeaturesSettings =
    FeaturesSettings(autoShutdownOnUnsupportedFeature = false, List.empty)
}

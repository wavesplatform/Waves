package com.wavesplatform.test

import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.settings.{FunctionalitySettings, WavesSettings, loadConfig}

object DomainPresets {
  implicit class WavesSettingsOps(val ws: WavesSettings) extends AnyVal {
    def configure(transformF: FunctionalitySettings => FunctionalitySettings): WavesSettings = {
      val functionalitySettings = transformF(ws.blockchainSettings.functionalitySettings)
      ws.copy(blockchainSettings = ws.blockchainSettings.copy(functionalitySettings = functionalitySettings))
    }

    def withFeatures(fs: BlockchainFeature*): WavesSettings =
      configure(_.copy(preActivatedFeatures = fs.map(_.id -> 0).toMap))

    def addFeatures(fs: BlockchainFeature*): WavesSettings = configure { functionalitySettings =>
      val newFeatures = functionalitySettings.preActivatedFeatures ++ fs.map(_.id -> 0)
      functionalitySettings.copy(preActivatedFeatures = newFeatures)
    }

    def setFeaturesHeight(fs: (BlockchainFeature, Int)*): WavesSettings = configure { functionalitySettings =>
      val newFeatures = functionalitySettings.preActivatedFeatures ++ fs.map { case (f, height) => (f.id, height) }
      functionalitySettings.copy(preActivatedFeatures = newFeatures)
    }

    def withActivationPeriod(period: Int): WavesSettings =
      configure(_.copy(featureCheckBlocksPeriod = period, blocksForFeatureActivation = period, doubleFeaturesPeriodsAfterHeight = 10000))

    def noFeatures(): WavesSettings = {
      ws.copy(
        blockchainSettings = ws.blockchainSettings.copy(
          functionalitySettings = ws.blockchainSettings.functionalitySettings
            .copy(preActivatedFeatures = Map.empty)
        ),
        featuresSettings = ws.featuresSettings.copy(supported = Nil)
      )
    }
  }

  lazy val SettingsFromDefaultConfig: WavesSettings = WavesSettings.fromRootConfig(loadConfig(None))

  def domainSettingsWithFS(fs: FunctionalitySettings): WavesSettings =
    SettingsFromDefaultConfig.copy(
      blockchainSettings = SettingsFromDefaultConfig.blockchainSettings.copy(functionalitySettings = fs)
    )

  def domainSettingsWithPreactivatedFeatures(fs: BlockchainFeature*): WavesSettings =
    domainSettingsWithFeatures(fs.map(_ -> 0)*)

  def domainSettingsWithFeatures(fs: (BlockchainFeature, Int)*): WavesSettings = {
    val defaultFS = SettingsFromDefaultConfig
      .noFeatures()
      .blockchainSettings
      .functionalitySettings

    domainSettingsWithFS(defaultFS.copy(preActivatedFeatures = fs.map { case (f, h) =>
      f.id -> h
    }.toMap))
  }

  val NG: WavesSettings = domainSettingsWithPreactivatedFeatures(
    BlockchainFeatures.MassTransfer, // Removes limit of 100 transactions per block
    BlockchainFeatures.NG
  )

  val ScriptsAndSponsorship: WavesSettings = NG
    .addFeatures(
      BlockchainFeatures.SmartAccounts,
      BlockchainFeatures.SmartAccountTrading,
      BlockchainFeatures.OrderV3,
      BlockchainFeatures.FeeSponsorship,
      BlockchainFeatures.DataTransaction,
      BlockchainFeatures.SmartAssets
    )
    .setFeaturesHeight(
      BlockchainFeatures.FeeSponsorship -> -NG.blockchainSettings.functionalitySettings.activationWindowSize(1)
    )

  val RideV3: WavesSettings = ScriptsAndSponsorship.addFeatures(
    BlockchainFeatures.Ride4DApps
  )

  val RideV4: WavesSettings = RideV3.addFeatures(
    BlockchainFeatures.BlockReward,
    BlockchainFeatures.BlockV5
  )

  val RideV5: WavesSettings = RideV4.addFeatures(BlockchainFeatures.SynchronousCalls)

  val RideV6: WavesSettings = RideV5.addFeatures(BlockchainFeatures.RideV6)

  val ConsensusImprovements: WavesSettings = RideV6.addFeatures(BlockchainFeatures.ConsensusImprovements)

  val BlockRewardDistribution: WavesSettings = ConsensusImprovements.addFeatures(BlockchainFeatures.BlockRewardDistribution)

  val ContinuationTransaction: WavesSettings = RideV6
    .addFeatures(BlockchainFeatures.ContinuationTransaction)
    .copy(
      featuresSettings = RideV6.featuresSettings.copy(autoShutdownOnUnsupportedFeature = false)
    )

  val TransactionStateSnapshot: WavesSettings = BlockRewardDistribution.addFeatures(BlockchainFeatures.TransactionStateSnapshot)

  def settingsForRide(version: StdLibVersion): WavesSettings =
    version match {
      case V1 => RideV3
      case V2 => RideV3
      case V3 => RideV3
      case V4 => RideV4
      case V5 => RideV5
      case V6 => RideV6
      case V7 => BlockRewardDistribution
    }

  def mostRecent: WavesSettings = RideV6
}

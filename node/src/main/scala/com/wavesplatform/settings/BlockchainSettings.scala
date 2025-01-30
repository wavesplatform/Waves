package com.wavesplatform.settings

import cats.syntax.either.*
import cats.syntax.traverse.*
import com.typesafe.config.Config
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import pureconfig.*

import scala.concurrent.duration.*

case class RewardsSettings(
    term: Int,
    termAfterCappedRewardFeature: Int,
    initial: Long,
    minIncrement: Long,
    votingInterval: Int
) derives ConfigReader {
  require(initial >= 0, "initial must be greater than or equal to 0")
  require(minIncrement > 0, "minIncrement must be greater than 0")
  require(term > 0, "term must be greater than 0")
  require(votingInterval > 0, "votingInterval must be greater than 0")
  require(votingInterval <= term, s"votingInterval must be less than or equal to term($term)")
  require(termAfterCappedRewardFeature > 0, "termAfterCappedRewardFeature must be greater than 0")
  require(
    votingInterval <= termAfterCappedRewardFeature,
    s"votingInterval must be less than or equal to termAfterCappedRewardFeature($termAfterCappedRewardFeature)"
  )

  def nearestTermEnd(activatedAt: Int, height: Int, modifyTerm: Boolean): Int = {
    require(height >= activatedAt)
    val diff         = height - activatedAt + 1
    val modifiedTerm = if (modifyTerm) termAfterCappedRewardFeature else term
    val mul          = math.ceil(diff.toDouble / modifiedTerm).toInt
    activatedAt + mul * modifiedTerm - 1
  }

  def votingWindow(activatedAt: Int, height: Int, modifyTerm: Boolean): Range = {
    val end   = nearestTermEnd(activatedAt, height, modifyTerm)
    val start = end - votingInterval + 1
    if (height >= start) Range.inclusive(start, height)
    else Range(0, 0)
  }
}

object RewardsSettings {
  val MAINNET, TESTNET, STAGENET = apply(
    100000,
    50000,
    6 * Constants.UnitsInWave,
    50000000,
    10000
  )
}

case class FunctionalitySettings(
    featureCheckBlocksPeriod: Int = defaultFeatureCheckBlocksPeriod,
    blocksForFeatureActivation: Int = defaultBlocksForFeatureActivation,
    generationBalanceDepthFrom50To1000AfterHeight: Int = defaultGenerationBalanceDepthFrom50To1000AfterHeight,
    blockVersion3AfterHeight: Int = defaultBlockVersion3AfterHeight,
    preActivatedFeatures: Map[Short, Int] = defaultPreActivatedFeatures,
    doubleFeaturesPeriodsAfterHeight: Int = defaultDoubleFeaturesPeriodsAfterHeight,
    maxTransactionTimeBackOffset: FiniteDuration = defaultMaxTransactionTimeBackOffset,
    maxTransactionTimeForwardOffset: FiniteDuration = defaultMaxTransactionTimeForwardOffset,
    lastTimeBasedForkParameter: Long = defaultLastTimeBasedForkParameter,
    leaseExpiration: Int = defaultLeaseExpiration,
    estimatorPreCheckHeight: Int = defaultEstimatorPreCheckHeight,
    minAssetInfoUpdateInterval: Int = defaultMinAssetInfoUpdateInterval,
    minBlockTime: FiniteDuration = defaultMinBlockTime,
    delayDelta: Int = defaultDelayDelta,
    estimationOverflowFixHeight: Int = defaultEstimationOverflowFixHeight,
    estimatorSumOverflowFixHeight: Int = defaultEstimatorSumOverflowFixHeight,
    enforceTransferValidationAfter: Int = defaultEnforceTransferValidationAfter,
    ethInvokePaymentsCheckHeight: Int = defaultEthInvokePaymentsCheckHeight,
    daoAddress: Option[String] = defaultDaoAddress,
    xtnBuybackAddress: Option[String] = defaultXtnBuybackAddress,
    xtnBuybackRewardPeriod: Int = defaultXtnBuybackRewardPeriod,
    lightNodeBlockFieldsAbsenceInterval: Int = defaultLightNodeBlockFieldsAbsenceInterval,
    blockRewardBoostPeriod: Int = defaultBlockRewardBoostPeriod,
    paymentsCheckHeight: Int = defaultPaymentsCheckHeight,
    unitsRegistryAddress: Option[String] = defaultUnitsRegistryAddress,
) {
  val allowLeasedBalanceTransferUntilHeight: Int              = blockVersion3AfterHeight
  val allowTemporaryNegativeUntil: Long                       = lastTimeBasedForkParameter
  val minimalGeneratingBalanceAfter: Long                     = lastTimeBasedForkParameter
  val allowTransactionsFromFutureUntil: Long                  = lastTimeBasedForkParameter
  val allowUnissuedAssetsUntil: Long                          = lastTimeBasedForkParameter
  val allowInvalidReissueInSameBlockUntilTimestamp: Long      = lastTimeBasedForkParameter
  val allowMultipleLeaseCancelTransactionUntilTimestamp: Long = lastTimeBasedForkParameter

  lazy val daoAddressParsed: Either[String, Option[Address]] =
    daoAddress.traverse(Address.fromString(_)).leftMap(_ => "Incorrect dao-address")
  lazy val xtnBuybackAddressParsed: Either[String, Option[Address]] =
    xtnBuybackAddress.traverse(Address.fromString(_)).leftMap(_ => "Incorrect xtn-buyback-address")
  lazy val unitsRegistryAddressParsed: Either[String, Option[Address]] =
    unitsRegistryAddress.traverse(Address.fromString(_)).leftMap(_ => "Incorrect units-registry-address")

  require(featureCheckBlocksPeriod > 0, "featureCheckBlocksPeriod must be greater than 0")
  require(
    (blocksForFeatureActivation > 0) && (blocksForFeatureActivation <= featureCheckBlocksPeriod),
    s"blocksForFeatureActivation must be in range 1 to $featureCheckBlocksPeriod"
  )
  require(minAssetInfoUpdateInterval >= 0, "minAssetInfoUpdateInterval must be greater than or equal to 0")

  def activationWindowSize(height: Int): Int =
    featureCheckBlocksPeriod * (if (height <= doubleFeaturesPeriodsAfterHeight) 1 else 2)

  def activationWindow(height: Int): Range =
    if (height < 1) Range(0, 0)
    else {
      val ws = activationWindowSize(height)
      Range.inclusive((height - 1) / ws * ws + 1, ((height - 1) / ws + 1) * ws)
    }

  def blocksForFeatureActivation(height: Int): Int =
    blocksForFeatureActivation * (if (height <= doubleFeaturesPeriodsAfterHeight) 1 else 2)

  def generatingBalanceDepth(height: Int): Int =
    if (height >= generationBalanceDepthFrom50To1000AfterHeight) 1000 else 50
}

object FunctionalitySettings {
  // Note: This setup (default values + manual ConfigReader instance) 
  // is a workaround for `pureconfig-generic-scala3` (it doesn't support default values from case classes yet)
  val defaultFeatureCheckBlocksPeriod: Int = 1000
  val defaultBlocksForFeatureActivation: Int = 800
  val defaultGenerationBalanceDepthFrom50To1000AfterHeight: Int = 0
  val defaultBlockVersion3AfterHeight: Int = 0
  val defaultPreActivatedFeatures: Map[Short, Int] = Map.empty
  val defaultDoubleFeaturesPeriodsAfterHeight: Int = Int.MaxValue
  val defaultMaxTransactionTimeBackOffset: FiniteDuration = 120.minutes
  val defaultMaxTransactionTimeForwardOffset: FiniteDuration = 90.minutes
  val defaultLastTimeBasedForkParameter: Long = 0L
  val defaultLeaseExpiration: Int = 1000000
  val defaultEstimatorPreCheckHeight: Int = 0
  val defaultMinAssetInfoUpdateInterval: Int = 100000
  val defaultMinBlockTime: FiniteDuration = 15.seconds
  val defaultDelayDelta: Int = 8
  val defaultEstimationOverflowFixHeight: Int = 0
  val defaultEstimatorSumOverflowFixHeight: Int = 0
  val defaultEnforceTransferValidationAfter: Int = 0
  val defaultEthInvokePaymentsCheckHeight: Int = 0
  val defaultDaoAddress: Option[String] = None
  val defaultXtnBuybackAddress: Option[String] = None
  val defaultXtnBuybackRewardPeriod: Int = Int.MaxValue
  val defaultLightNodeBlockFieldsAbsenceInterval: Int = 1000
  val defaultBlockRewardBoostPeriod: Int = 1000
  val defaultPaymentsCheckHeight: Int = 0
  val defaultUnitsRegistryAddress: Option[String] = None

  given ConfigReader[FunctionalitySettings] = ConfigReader.fromCursor(cur =>
    for {
      objCur <- cur.asObjectCursor
      featureCheckBlocksPeriod <- objCur.optionalWithDefault("feature-check-blocks-period", defaultFeatureCheckBlocksPeriod)
      blocksForFeatureActivation <- objCur.optionalWithDefault("blocks-for-feature-activation", defaultBlocksForFeatureActivation)
      generationBalanceDepthFrom50To1000AfterHeight <- objCur.optionalWithDefault("generation-balance-depth-from-50-to-1000-after-height", defaultGenerationBalanceDepthFrom50To1000AfterHeight)
      blockVersion3AfterHeight <- objCur.optionalWithDefault("block-version-3-after-height", defaultBlockVersion3AfterHeight)
      preActivatedFeatures <- objCur.optionalWithDefault("pre-activated-features", defaultPreActivatedFeatures)
      doubleFeaturesPeriodsAfterHeight <- objCur.optionalWithDefault("double-features-periods-after-height", defaultDoubleFeaturesPeriodsAfterHeight)
      maxTransactionTimeBackOffset <- objCur.optionalWithDefault("max-transaction-time-back-offset", defaultMaxTransactionTimeBackOffset)
      maxTransactionTimeForwardOffset <- objCur.optionalWithDefault("max-transaction-time-forward-offset", defaultMaxTransactionTimeForwardOffset)
      lastTimeBasedForkParameter <- objCur.optionalWithDefault("last-time-based-fork-parameter", defaultLastTimeBasedForkParameter)
      leaseExpiration <- objCur.optionalWithDefault("lease-expiration", defaultLeaseExpiration)
      estimatorPreCheckHeight <- objCur.optionalWithDefault("estimator-pre-check-height", defaultEstimatorPreCheckHeight)
      minAssetInfoUpdateInterval <- objCur.optionalWithDefault("min-asset-info-update-interval", defaultMinAssetInfoUpdateInterval)
      minBlockTime <- objCur.optionalWithDefault("min-block-time", defaultMinBlockTime)
      delayDelta <- objCur.optionalWithDefault("delay-delta", defaultDelayDelta)
      estimationOverflowFixHeight <- objCur.optionalWithDefault("estimation-overflow-fix-height", defaultEstimationOverflowFixHeight)
      estimatorSumOverflowFixHeight <- objCur.optionalWithDefault("estimator-sum-overflow-fix-height", defaultEstimatorSumOverflowFixHeight)
      enforceTransferValidationAfter <- objCur.optionalWithDefault("enforce-transfer-validation-after", defaultEnforceTransferValidationAfter)
      ethInvokePaymentsCheckHeight <- objCur.optionalWithDefault("eth-invoke-payments-check-height", defaultEthInvokePaymentsCheckHeight)
      daoAddress <- objCur.optionalWithDefault("dao-address", defaultDaoAddress)
      xtnBuybackAddress <- objCur.optionalWithDefault("xtn-buyback-address", defaultXtnBuybackAddress)
      xtnBuybackRewardPeriod <- objCur.optionalWithDefault("xtn-buyback-reward-period", defaultXtnBuybackRewardPeriod)
      lightNodeBlockFieldsAbsenceInterval <- objCur.optionalWithDefault("light-node-block-fields-absence-interval", defaultLightNodeBlockFieldsAbsenceInterval)
      blockRewardBoostPeriod <- objCur.optionalWithDefault("block-reward-boost-period", defaultBlockRewardBoostPeriod)
      paymentsCheckHeight <- objCur.optionalWithDefault("payments-check-height", defaultPaymentsCheckHeight)
      unitsRegistryAddress <- objCur.optionalWithDefault("units-registry-address", defaultUnitsRegistryAddress)
    } yield FunctionalitySettings(
      featureCheckBlocksPeriod,
      blocksForFeatureActivation,
      generationBalanceDepthFrom50To1000AfterHeight,
      blockVersion3AfterHeight,
      preActivatedFeatures,
      doubleFeaturesPeriodsAfterHeight,
      maxTransactionTimeBackOffset,
      maxTransactionTimeForwardOffset,
      lastTimeBasedForkParameter,
      leaseExpiration,
      estimatorPreCheckHeight,
      minAssetInfoUpdateInterval,
      minBlockTime,
      delayDelta,
      estimationOverflowFixHeight,
      estimatorSumOverflowFixHeight,
      enforceTransferValidationAfter,
      ethInvokePaymentsCheckHeight,
      daoAddress,
      xtnBuybackAddress,
      xtnBuybackRewardPeriod,
      lightNodeBlockFieldsAbsenceInterval,
      blockRewardBoostPeriod,
      paymentsCheckHeight,
      unitsRegistryAddress
    )
  )

  val MAINNET: FunctionalitySettings = apply(
    featureCheckBlocksPeriod = 5000,
    blocksForFeatureActivation = 4000,
    generationBalanceDepthFrom50To1000AfterHeight = 232000,
    blockVersion3AfterHeight = 795000,
    doubleFeaturesPeriodsAfterHeight = 810000,
    lastTimeBasedForkParameter = 1530161445559L,
    estimatorPreCheckHeight = 1847610,
    estimationOverflowFixHeight = 2858710,
    estimatorSumOverflowFixHeight = 2897510,
    enforceTransferValidationAfter = 2959447,
    daoAddress = Some("3PEgG7eZHLFhcfsTSaYxgRhZsh4AxMvA4Ms"),
    xtnBuybackAddress = Some("3PFjHWuH6WXNJbwnfLHqNFBpwBS5dkYjTfv"),
    xtnBuybackRewardPeriod = 100000,
    blockRewardBoostPeriod = 300_000,
    paymentsCheckHeight = 4303300,
    unitsRegistryAddress = Some("3P8LfPXcveST7WKkV3UACQNdr6J3shPYong")
  )

  val TESTNET: FunctionalitySettings = apply(
    featureCheckBlocksPeriod = 3000,
    blocksForFeatureActivation = 2700,
    blockVersion3AfterHeight = 161700,
    doubleFeaturesPeriodsAfterHeight = Int.MaxValue,
    lastTimeBasedForkParameter = 1492560000000L,
    estimatorPreCheckHeight = 817380,
    estimationOverflowFixHeight = 1793770,
    estimatorSumOverflowFixHeight = 1832520,
    enforceTransferValidationAfter = 1698800,
    daoAddress = Some("3Myb6G8DkdBb8YcZzhrky65HrmiNuac3kvS"),
    xtnBuybackAddress = Some("3N13KQpdY3UU7JkWUBD9kN7t7xuUgeyYMTT"),
    xtnBuybackRewardPeriod = 2000,
    blockRewardBoostPeriod = 2_000,
    unitsRegistryAddress = Some("3N9fwNGJcUcAbhh7YPr6mrpuGJD4tApZFsT")
  )

  val STAGENET: FunctionalitySettings = apply(
    featureCheckBlocksPeriod = 100,
    blocksForFeatureActivation = 40,
    preActivatedFeatures = (1 to 13).map(_.toShort -> 0).toMap,
    doubleFeaturesPeriodsAfterHeight = 1000000000,
    minAssetInfoUpdateInterval = 10,
    estimationOverflowFixHeight = 1078680,
    estimatorSumOverflowFixHeight = 1097419,
    ethInvokePaymentsCheckHeight = 1311110,
    daoAddress = Some("3MaFVH1vTv18FjBRugSRebx259D7xtRh9ic"),
    xtnBuybackAddress = Some("3MbhiRiLFLJ1EVKNP9npRszcLLQDjwnFfZM"),
    xtnBuybackRewardPeriod = 1000,
    paymentsCheckHeight = 2195900
  )
}

case class GenesisTransactionSettings(recipient: String, amount: Long) derives ConfigReader

case class GenesisSettings(
    blockTimestamp: Long,
    timestamp: Long,
    initialBalance: Long,
    signature: Option[ByteStr],
    transactions: Seq[GenesisTransactionSettings],
    initialBaseTarget: Long,
    averageBlockDelay: FiniteDuration
) derives ConfigReader

object GenesisSettings { // TODO: Move to network-defaults.conf
  val MAINNET: GenesisSettings = GenesisSettings(
    1460678400000L,
    1465742577614L,
    Constants.UnitsInWave * Constants.TotalWaves,
    ByteStr.decodeBase58("FSH8eAAzZNqnG8xgTZtz5xuLqXySsXgAjmFEC25hXMbEufiGjqWPnGCZFt6gLiVLJny16ipxRNAkkzjjhqTjBE2").toOption,
    List(
      GenesisTransactionSettings("3PAWwWa6GbwcJaFzwqXQN5KQm7H96Y7SHTQ", Constants.UnitsInWave * Constants.TotalWaves - 5 * Constants.UnitsInWave),
      GenesisTransactionSettings("3P8JdJGYc7vaLu4UXUZc1iRLdzrkGtdCyJM", Constants.UnitsInWave),
      GenesisTransactionSettings("3PAGPDPqnGkyhcihyjMHe9v36Y4hkAh9yDy", Constants.UnitsInWave),
      GenesisTransactionSettings("3P9o3ZYwtHkaU1KxsKkFjJqJKS3dLHLC9oF", Constants.UnitsInWave),
      GenesisTransactionSettings("3PJaDyprvekvPXPuAtxrapacuDJopgJRaU3", Constants.UnitsInWave),
      GenesisTransactionSettings("3PBWXDFUc86N2EQxKJmW8eFco65xTyMZx6J", Constants.UnitsInWave)
    ),
    153722867L,
    60.seconds
  )

  val TESTNET: GenesisSettings = GenesisSettings(
    1460678400000L,
    1478000000000L,
    Constants.UnitsInWave * Constants.TotalWaves,
    ByteStr.decodeBase58("5uqnLK3Z9eiot6FyYBfwUnbyid3abicQbAZjz38GQ1Q8XigQMxTK4C1zNkqS1SVw7FqSidbZKxWAKLVoEsp4nNqa").toOption,
    List(
      GenesisTransactionSettings("3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8", (Constants.UnitsInWave * Constants.TotalWaves * 0.04).toLong),
      GenesisTransactionSettings("3NBVqYXrapgJP9atQccdBPAgJPwHDKkh6A8", (Constants.UnitsInWave * Constants.TotalWaves * 0.02).toLong),
      GenesisTransactionSettings("3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh", (Constants.UnitsInWave * Constants.TotalWaves * 0.02).toLong),
      GenesisTransactionSettings("3NCBMxgdghg4tUhEEffSXy11L6hUi6fcBpd", (Constants.UnitsInWave * Constants.TotalWaves * 0.02).toLong),
      GenesisTransactionSettings(
        "3N18z4B8kyyQ96PhN5eyhCAbg4j49CgwZJx",
        (Constants.UnitsInWave * Constants.TotalWaves - Constants.UnitsInWave * Constants.TotalWaves * 0.1).toLong
      )
    ),
    153722867L,
    60.seconds
  )

  val STAGENET: GenesisSettings = GenesisSettings(
    1561705836768L,
    1561705836768L,
    Constants.UnitsInWave * Constants.TotalWaves,
    ByteStr.decodeBase58("2EaaguFPgrJ1bbMAFrPw2bi6i7kqjgvxsFj8YGqrKR7hT54ZvwmzZ3LHMm4qR7i7QB5cacp8XdkLMJyvjFkt8VgN").toOption,
    List(
      GenesisTransactionSettings("3Mi63XiwniEj6mTC557pxdRDddtpj7fZMMw", Constants.UnitsInWave * Constants.TotalWaves)
    ),
    5000,
    1.minute
  )
}

case class BlockchainSettings(
    addressSchemeCharacter: Char,
    functionalitySettings: FunctionalitySettings,
    genesisSettings: GenesisSettings,
    rewardsSettings: RewardsSettings
)

private[settings] object BlockchainType {
  val STAGENET = "STAGENET"
  val TESTNET  = "TESTNET"
  val MAINNET  = "MAINNET"
}

object BlockchainSettings {
  def fromRootConfig(config: Config): BlockchainSettings =
    ConfigSource.fromConfig(config).at("waves.blockchain").loadOrThrow[BlockchainSettings]

  given ConfigReader[BlockchainSettings] = ConfigReader.fromCursor(cur =>
    for {
      objCur               <- cur.asObjectCursor
      blockchainTypeString <- objCur.atKey("type").flatMap(_.asString).map(_.toUpperCase)
      (addressSchemeCharacter, functionalitySettings, genesisSettings, rewardsSettings) <- blockchainTypeString match {
        case BlockchainType.STAGENET => Right(('S', FunctionalitySettings.STAGENET, GenesisSettings.STAGENET, RewardsSettings.STAGENET))
        case BlockchainType.TESTNET  => Right(('T', FunctionalitySettings.TESTNET, GenesisSettings.TESTNET, RewardsSettings.TESTNET))
        case BlockchainType.MAINNET  => Right(('W', FunctionalitySettings.MAINNET, GenesisSettings.MAINNET, RewardsSettings.MAINNET))
        case _                       =>
          // Custom
          for {
            customObjCur  <- objCur.atKey("custom").flatMap(_.asObjectCursor)
            networkId     <- customObjCur.atKey("address-scheme-character").flatMap(_.asString).map(_.charAt(0))
            functionality <- customObjCur.atKey("functionality").flatMap(ConfigReader[FunctionalitySettings].from)
            genesis       <- customObjCur.atKey("genesis").flatMap(ConfigReader[GenesisSettings].from)
            rewards       <- customObjCur.atKey("rewards").flatMap(ConfigReader[RewardsSettings].from)
          } yield {
            require(functionality.minBlockTime <= genesis.averageBlockDelay, "minBlockTime should be <= averageBlockDelay")
            (networkId, functionality, genesis, rewards)
          }
      }

    } yield BlockchainSettings(addressSchemeCharacter, functionalitySettings, genesisSettings, rewardsSettings)
  )
}

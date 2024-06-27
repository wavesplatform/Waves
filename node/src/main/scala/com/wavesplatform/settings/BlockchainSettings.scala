package com.wavesplatform.settings

import cats.syntax.either.*
import cats.syntax.traverse.*
import com.typesafe.config.Config
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import net.ceedubs.ficus.Ficus.*
import net.ceedubs.ficus.readers.ArbitraryTypeReader.*
import net.ceedubs.ficus.readers.ValueReader

import scala.concurrent.duration.*

case class RewardsSettings(
    term: Int,
    termAfterCappedRewardFeature: Int,
    initial: Long,
    minIncrement: Long,
    votingInterval: Int
) {
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
    featureCheckBlocksPeriod: Int = 1000,
    blocksForFeatureActivation: Int = 800,
    generationBalanceDepthFrom50To1000AfterHeight: Int = 0,
    blockVersion3AfterHeight: Int = 0,
    preActivatedFeatures: Map[Short, Int] = Map.empty,
    doubleFeaturesPeriodsAfterHeight: Int = Int.MaxValue,
    maxTransactionTimeBackOffset: FiniteDuration = 120.minutes,
    maxTransactionTimeForwardOffset: FiniteDuration = 90.minutes,
    lastTimeBasedForkParameter: Long = 0L,
    leaseExpiration: Int = 1000000,
    estimatorPreCheckHeight: Int = 0,
    minAssetInfoUpdateInterval: Int = 100000,
    minBlockTime: FiniteDuration = 15.seconds,
    delayDelta: Int = 8,
    estimationOverflowFixHeight: Int = 0,
    estimatorSumOverflowFixHeight: Int = 0,
    enforceTransferValidationAfter: Int = 0,
    ethInvokePaymentsCheckHeight: Int = 0,
    daoAddress: Option[String] = None,
    xtnBuybackAddress: Option[String] = None,
    xtnBuybackRewardPeriod: Int = Int.MaxValue,
    lightNodeBlockFieldsAbsenceInterval: Int = 1000,
    blockRewardBoostPeriod: Int = 1000
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
    blockRewardBoostPeriod = 300_000
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
    xtnBuybackRewardPeriod = 2000
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
    xtnBuybackRewardPeriod = 1000
  )
}

case class GenesisTransactionSettings(recipient: String, amount: Long)

case class GenesisSettings(
    blockTimestamp: Long,
    timestamp: Long,
    initialBalance: Long,
    signature: Option[ByteStr],
    transactions: Seq[GenesisTransactionSettings],
    initialBaseTarget: Long,
    averageBlockDelay: FiniteDuration
)

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
  implicit val valueReader: ValueReader[BlockchainSettings] =
    (cfg: Config, path: String) => fromConfig(cfg.getConfig(path))

  // @deprecated("Use config.as[BlockchainSettings]", "0.17.0")
  def fromRootConfig(config: Config): BlockchainSettings = config.as[BlockchainSettings]("waves.blockchain")

  private[this] def fromConfig(config: Config): BlockchainSettings = {
    val blockchainType = config.as[String]("type").toUpperCase
    val (addressSchemeCharacter, functionalitySettings, genesisSettings, rewardsSettings) = blockchainType match {
      case BlockchainType.STAGENET =>
        ('S', FunctionalitySettings.STAGENET, GenesisSettings.STAGENET, RewardsSettings.STAGENET)
      case BlockchainType.TESTNET =>
        ('T', FunctionalitySettings.TESTNET, GenesisSettings.TESTNET, RewardsSettings.TESTNET)
      case BlockchainType.MAINNET =>
        ('W', FunctionalitySettings.MAINNET, GenesisSettings.MAINNET, RewardsSettings.MAINNET)
      case _ => // Custom
        val networkId     = config.as[String](s"custom.address-scheme-character").charAt(0)
        val functionality = config.as[FunctionalitySettings](s"custom.functionality")
        val genesis       = config.as[GenesisSettings](s"custom.genesis")
        val rewards       = config.as[RewardsSettings](s"custom.rewards")
        require(functionality.minBlockTime <= genesis.averageBlockDelay, "minBlockTime should be <= averageBlockDelay")
        (networkId, functionality, genesis, rewards)
    }

    BlockchainSettings(
      addressSchemeCharacter = addressSchemeCharacter,
      functionalitySettings = functionalitySettings,
      genesisSettings = genesisSettings,
      rewardsSettings = rewardsSettings
    )
  }
}

package com.wavesplatform.settings

import com.typesafe.config.Config
import com.wavesplatform.common.state.ByteStr
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.EnumerationReader._
import net.ceedubs.ficus.readers.ValueReader

import scala.concurrent.duration._

case class RewardsSettings(
    term: Int,
    initial: Long,
    minIncrement: Long,
    votingInterval: Int
) {
  require(initial >= 0, "initial must be greater than or equal to 0")
  require(minIncrement > 0, "minIncrement must be greater than 0")
  require(term > 0, "term must be greater than 0")
  require(votingInterval > 0, "votingInterval must be greater than 0")
  require(votingInterval <= term, s"votingInterval must be less than or equal to term($term)")

  def nearestTermEnd(activatedAt: Int, height: Int): Int = {
    require(height >= activatedAt)
    val diff = height - activatedAt + 1
    val mul  = math.ceil(diff.toDouble / term).toInt
    activatedAt + mul * term - 1
  }

  def votingWindow(activatedAt: Int, height: Int): Range = {
    val end   = nearestTermEnd(activatedAt, height)
    val start = end - votingInterval + 1
    if (height >= start) Range.inclusive(start, height)
    else Range(0, 0)
  }
}

object RewardsSettings {
  val MAINNET = apply(
    100000,
    6 * Constants.UnitsInWave,
    50000000,
    10000
  )

  val TESTNET = apply(
    100000,
    6 * Constants.UnitsInWave,
    50000000,
    10000
  )

  val STAGENET = apply(
    100000,
    6 * Constants.UnitsInWave,
    50000000,
    10000
  )
}

case class FunctionalitySettings(
    featureCheckBlocksPeriod: Int,
    blocksForFeatureActivation: Int,
    generationBalanceDepthFrom50To1000AfterHeight: Int = 0,
    resetEffectiveBalancesAtHeight: Int = 0,
    blockVersion3AfterHeight: Int = 0,
    preActivatedFeatures: Map[Short, Int] = Map.empty,
    doubleFeaturesPeriodsAfterHeight: Int,
    maxTransactionTimeBackOffset: FiniteDuration = 120.minutes,
    maxTransactionTimeForwardOffset: FiniteDuration = 90.minutes,
    lastTimeBasedForkParameter: Long = 0L,
    leaseExpiration: Int = 1000000,
    estimatorPreCheckHeight: Int = 0,
    resetInvalidInvokeSponsoredFeeHeight: Int = 0
) {
  val allowLeasedBalanceTransferUntilHeight: Int        = blockVersion3AfterHeight
  val allowTemporaryNegativeUntil                       = lastTimeBasedForkParameter
  val minimalGeneratingBalanceAfter                     = lastTimeBasedForkParameter
  val allowTransactionsFromFutureUntil                  = lastTimeBasedForkParameter
  val allowUnissuedAssetsUntil                          = lastTimeBasedForkParameter
  val allowInvalidReissueInSameBlockUntilTimestamp      = lastTimeBasedForkParameter
  val allowMultipleLeaseCancelTransactionUntilTimestamp = lastTimeBasedForkParameter

  require(featureCheckBlocksPeriod > 0, "featureCheckBlocksPeriod must be greater than 0")
  require(
    (blocksForFeatureActivation > 0) && (blocksForFeatureActivation <= featureCheckBlocksPeriod),
    s"blocksForFeatureActivation must be in range 1 to $featureCheckBlocksPeriod"
  )

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
  val MAINNET = apply(
    featureCheckBlocksPeriod = 5000,
    blocksForFeatureActivation = 4000,
    generationBalanceDepthFrom50To1000AfterHeight = 232000,
    lastTimeBasedForkParameter = 1530161445559L,
    resetEffectiveBalancesAtHeight = 462000,
    blockVersion3AfterHeight = 795000,
    doubleFeaturesPeriodsAfterHeight = 810000,
    estimatorPreCheckHeight = 1847610,
    resetInvalidInvokeSponsoredFeeHeight = 1950100
  )

  val TESTNET = apply(
    featureCheckBlocksPeriod = 3000,
    blocksForFeatureActivation = 2700,
    resetEffectiveBalancesAtHeight = 51500,
    blockVersion3AfterHeight = 161700,
    doubleFeaturesPeriodsAfterHeight = Int.MaxValue,
    lastTimeBasedForkParameter = 1492560000000L,
    estimatorPreCheckHeight = 817380
  )

  val STAGENET = apply(
    featureCheckBlocksPeriod = 100,
    blocksForFeatureActivation = 40,
    doubleFeaturesPeriodsAfterHeight = 1000000000,
    preActivatedFeatures = (1 to 13).map(_.toShort -> 0).toMap
  )

  val configPath = "waves.blockchain.custom.functionality"
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

object GenesisSettings {
  val MAINNET = GenesisSettings(
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

  val TESTNET = GenesisSettings(
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

  val STAGENET = GenesisSettings(
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

object BlockchainType extends Enumeration {
  val STAGENET = Value("STAGENET")
  val TESTNET  = Value("TESTNET")
  val MAINNET  = Value("MAINNET")
  val CUSTOM   = Value("CUSTOM")
}

object BlockchainSettings {
  implicit val valueReader: ValueReader[BlockchainSettings] =
    (cfg: Config, path: String) => fromConfig(cfg.getConfig(path))

  // @deprecated("Use config.as[BlockchainSettings]", "0.17.0")
  def fromRootConfig(config: Config): BlockchainSettings = config.as[BlockchainSettings]("waves.blockchain")

  private[this] def fromConfig(config: Config): BlockchainSettings = {
    val blockchainType = config.as[BlockchainType.Value]("type")
    val (addressSchemeCharacter, functionalitySettings, genesisSettings, rewardsSettings) = blockchainType match {
      case BlockchainType.STAGENET =>
        ('S', FunctionalitySettings.STAGENET, GenesisSettings.STAGENET, RewardsSettings.STAGENET)
      case BlockchainType.TESTNET =>
        ('T', FunctionalitySettings.TESTNET, GenesisSettings.TESTNET, RewardsSettings.TESTNET)
      case BlockchainType.MAINNET =>
        ('W', FunctionalitySettings.MAINNET, GenesisSettings.MAINNET, RewardsSettings.MAINNET)
      case BlockchainType.CUSTOM =>
        val addressSchemeCharacter = config.as[String](s"custom.address-scheme-character").charAt(0)
        val functionalitySettings  = config.as[FunctionalitySettings](s"custom.functionality")
        val genesisSettings        = config.as[GenesisSettings](s"custom.genesis")
        val rewardsSettings        = config.as[RewardsSettings](s"custom.rewards")
        (addressSchemeCharacter, functionalitySettings, genesisSettings, rewardsSettings)
    }

    BlockchainSettings(
      addressSchemeCharacter = addressSchemeCharacter,
      functionalitySettings = functionalitySettings,
      genesisSettings = genesisSettings,
      rewardsSettings = rewardsSettings
    )
  }
}

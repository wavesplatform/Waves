package com.wavesplatform.settings

import com.typesafe.config.Config
import com.wavesplatform.common.state.ByteStr
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.EnumerationReader._
import net.ceedubs.ficus.readers.ValueReader

import scala.concurrent.duration._

case class FunctionalitySettings(featureCheckBlocksPeriod: Int,
                                 blocksForFeatureActivation: Int,
                                 allowTemporaryNegativeUntil: Long,
                                 generationBalanceDepthFrom50To1000AfterHeight: Int,
                                 minimalGeneratingBalanceAfter: Long,
                                 allowTransactionsFromFutureUntil: Long,
                                 allowUnissuedAssetsUntil: Long,
                                 allowInvalidReissueInSameBlockUntilTimestamp: Long,
                                 allowMultipleLeaseCancelTransactionUntilTimestamp: Long,
                                 resetEffectiveBalancesAtHeight: Int,
                                 blockVersion3AfterHeight: Int,
                                 preActivatedFeatures: Map[Short, Int],
                                 doubleFeaturesPeriodsAfterHeight: Int,
                                 maxTransactionTimeBackOffset: FiniteDuration,
                                 maxTransactionTimeForwardOffset: FiniteDuration) {
  val allowLeasedBalanceTransferUntilHeight: Int = blockVersion3AfterHeight

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
    allowTemporaryNegativeUntil = 1479168000000L,
    generationBalanceDepthFrom50To1000AfterHeight = 232000,
    minimalGeneratingBalanceAfter = 1479168000000L,
    allowTransactionsFromFutureUntil = 1479168000000L,
    allowUnissuedAssetsUntil = 1479416400000L,
    allowInvalidReissueInSameBlockUntilTimestamp = 1530161445559L, // 1492768800000L
    allowMultipleLeaseCancelTransactionUntilTimestamp = 1492768800000L,
    resetEffectiveBalancesAtHeight = 462000,
    blockVersion3AfterHeight = 795000,
    preActivatedFeatures = Map.empty,
    doubleFeaturesPeriodsAfterHeight = 810000,
    maxTransactionTimeBackOffset = 120.minutes,
    maxTransactionTimeForwardOffset = 90.minutes
  )

  val TESTNET = apply(
    featureCheckBlocksPeriod = 3000,
    blocksForFeatureActivation = 2700,
    allowTemporaryNegativeUntil = 1477958400000L,
    generationBalanceDepthFrom50To1000AfterHeight = 0,
    minimalGeneratingBalanceAfter = 0,
    allowTransactionsFromFutureUntil = 1478100000000L,
    allowUnissuedAssetsUntil = 1479416400000L,
    allowInvalidReissueInSameBlockUntilTimestamp = 1492560000000L,
    allowMultipleLeaseCancelTransactionUntilTimestamp = 1492560000000L,
    resetEffectiveBalancesAtHeight = 51500,
    blockVersion3AfterHeight = 161700,
    preActivatedFeatures = Map.empty,
    doubleFeaturesPeriodsAfterHeight = Int.MaxValue,
    maxTransactionTimeBackOffset = 120.minutes,
    maxTransactionTimeForwardOffset = 90.minutes
  )

  val configPath = "waves.blockchain.custom.functionality"
}

case class GenesisTransactionSettings(recipient: String, amount: Long)

case class GenesisSettings(blockTimestamp: Long,
                           timestamp: Long,
                           initialBalance: Long,
                           signature: Option[ByteStr],
                           transactions: Seq[GenesisTransactionSettings],
                           initialBaseTarget: Long,
                           averageBlockDelay: FiniteDuration)

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
      GenesisTransactionSettings("3N18z4B8kyyQ96PhN5eyhCAbg4j49CgwZJx",
                                 (Constants.UnitsInWave * Constants.TotalWaves - Constants.UnitsInWave * Constants.TotalWaves * 0.1).toLong)
    ),
    153722867L,
    60.seconds
  )
}

case class BlockchainSettings(addressSchemeCharacter: Char, functionalitySettings: FunctionalitySettings, genesisSettings: GenesisSettings)

object BlockchainType extends Enumeration {
  val TESTNET = Value("TESTNET")
  val MAINNET = Value("MAINNET")
  val CUSTOM  = Value("CUSTOM")
}

object BlockchainSettings {
  implicit val valueReader: ValueReader[BlockchainSettings] =
    (cfg: Config, path: String) => fromConfig(cfg.getConfig(path))

  // @deprecated("Use config.as[BlockchainSettings]", "0.17.0")
  def fromRootConfig(config: Config): BlockchainSettings = config.as[BlockchainSettings]("waves.blockchain")

  private[this] def fromConfig(config: Config): BlockchainSettings = {
    val blockchainType = config.as[BlockchainType.Value]("type")
    val (addressSchemeCharacter, functionalitySettings, genesisSettings) = blockchainType match {
      case BlockchainType.TESTNET =>
        ('T', FunctionalitySettings.TESTNET, GenesisSettings.TESTNET)
      case BlockchainType.MAINNET =>
        ('W', FunctionalitySettings.MAINNET, GenesisSettings.MAINNET)
      case BlockchainType.CUSTOM =>
        val addressSchemeCharacter = config.as[String](s"custom.address-scheme-character").charAt(0)
        val functionalitySettings  = config.as[FunctionalitySettings](s"custom.functionality")
        val genesisSettings        = config.as[GenesisSettings](s"custom.genesis")
        (addressSchemeCharacter, functionalitySettings, genesisSettings)
    }

    BlockchainSettings(
      addressSchemeCharacter = addressSchemeCharacter,
      functionalitySettings = functionalitySettings,
      genesisSettings = genesisSettings
    )
  }
}

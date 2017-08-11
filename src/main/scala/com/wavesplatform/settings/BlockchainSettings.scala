package com.wavesplatform.settings

import java.io.File

import com.typesafe.config.Config
import com.wavesplatform.state2.ByteStr
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.EnumerationReader._

import scala.concurrent.duration._

case class FunctionalitySettings(allowTemporaryNegativeUntil: Long,
                                 allowInvalidPaymentTransactionsByTimestamp: Long,
                                 requireSortedTransactionsAfter: Long,
                                 generationBalanceDepthFrom50To1000AfterHeight: Long,
                                 minimalGeneratingBalanceAfter: Long,
                                 allowTransactionsFromFutureUntil: Long,
                                 allowUnissuedAssetsUntil: Long,
                                 allowBurnTransactionAfter: Long,
                                 allowLeaseTransactionAfter: Long,
                                 requirePaymentUniqueIdAfter: Long,
                                 allowExchangeTransactionAfter: Long,
                                 allowInvalidReissueInSameBlockUntilTimestamp: Long,
                                 allowCreatealiasTransactionAfter: Long,
                                 allowMultipleLeaseCancelTransactionUntilTimestamp: Long,
                                 resetEffectiveBalancesAtHeight: Long,
                                 allowLeasedBalanceTransferUntil: Long,
                                 enableMicroblocksAfter: Long)

object FunctionalitySettings {
  val MAINNET = FunctionalitySettings(allowTemporaryNegativeUntil = 1479168000000L,
    allowInvalidPaymentTransactionsByTimestamp = 1479168000000L,
    requireSortedTransactionsAfter = 1479168000000L,
    generationBalanceDepthFrom50To1000AfterHeight = 232000L,
    minimalGeneratingBalanceAfter = 1479168000000L,
    allowTransactionsFromFutureUntil = 1479168000000L,
    allowUnissuedAssetsUntil = 1479416400000L,
    allowBurnTransactionAfter = 1491192000000L,
    allowLeaseTransactionAfter = 1491192000000L,
    requirePaymentUniqueIdAfter = 1491192000000L,
    allowExchangeTransactionAfter = 1491192000000L,
    allowInvalidReissueInSameBlockUntilTimestamp = 1492768800000L,
    allowCreatealiasTransactionAfter = Long.MaxValue,
    allowMultipleLeaseCancelTransactionUntilTimestamp = 1492768800000L,
    resetEffectiveBalancesAtHeight = 462000,
    allowLeasedBalanceTransferUntil = Long.MaxValue,
    enableMicroblocksAfter = Long.MaxValue
  )

  val TESTNET = FunctionalitySettings(
    allowTemporaryNegativeUntil = 1477958400000L,
    allowInvalidPaymentTransactionsByTimestamp = 1477958400000L,
    requireSortedTransactionsAfter = 1477958400000L,
    generationBalanceDepthFrom50To1000AfterHeight = Long.MinValue,
    minimalGeneratingBalanceAfter = Long.MinValue,
    allowTransactionsFromFutureUntil = 1478100000000L,
    allowUnissuedAssetsUntil = 1479416400000L,
    allowBurnTransactionAfter = 1481110521000L,
    allowLeaseTransactionAfter = Long.MinValue,
    requirePaymentUniqueIdAfter = 1485942685000L,
    allowExchangeTransactionAfter = 1483228800000L,
    allowInvalidReissueInSameBlockUntilTimestamp = 1492560000000L,
    allowCreatealiasTransactionAfter = 1493596800000L,
    allowMultipleLeaseCancelTransactionUntilTimestamp = 1492560000000L,
    resetEffectiveBalancesAtHeight = 51500,
    allowLeasedBalanceTransferUntil = 1495238400000L,
    enableMicroblocksAfter = Long.MaxValue)

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
  averageBlockDelay: FiniteDuration)

object GenesisSettings {
  val MAINNET = GenesisSettings(1460678400000L, 1465742577614L, Constants.UnitsInWave * Constants.TotalWaves,
    ByteStr.decodeBase58("FSH8eAAzZNqnG8xgTZtz5xuLqXySsXgAjmFEC25hXMbEufiGjqWPnGCZFt6gLiVLJny16ipxRNAkkzjjhqTjBE2").toOption,
    List(
      GenesisTransactionSettings("3PAWwWa6GbwcJaFzwqXQN5KQm7H96Y7SHTQ", Constants.UnitsInWave * Constants.TotalWaves - 5 * Constants.UnitsInWave),
      GenesisTransactionSettings("3P8JdJGYc7vaLu4UXUZc1iRLdzrkGtdCyJM", Constants.UnitsInWave),
      GenesisTransactionSettings("3PAGPDPqnGkyhcihyjMHe9v36Y4hkAh9yDy", Constants.UnitsInWave),
      GenesisTransactionSettings("3P9o3ZYwtHkaU1KxsKkFjJqJKS3dLHLC9oF", Constants.UnitsInWave),
      GenesisTransactionSettings("3PJaDyprvekvPXPuAtxrapacuDJopgJRaU3", Constants.UnitsInWave),
      GenesisTransactionSettings("3PBWXDFUc86N2EQxKJmW8eFco65xTyMZx6J", Constants.UnitsInWave)),
    153722867L, 60.seconds)

  val TESTNET = GenesisSettings(1460678400000L, 1478000000000L, Constants.UnitsInWave * Constants.TotalWaves,
    ByteStr.decodeBase58("5uqnLK3Z9eiot6FyYBfwUnbyid3abicQbAZjz38GQ1Q8XigQMxTK4C1zNkqS1SVw7FqSidbZKxWAKLVoEsp4nNqa").toOption,
    List(
      GenesisTransactionSettings("3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8", (Constants.UnitsInWave * Constants.TotalWaves * 0.04).toLong),
      GenesisTransactionSettings("3NBVqYXrapgJP9atQccdBPAgJPwHDKkh6A8", (Constants.UnitsInWave * Constants.TotalWaves * 0.02).toLong),
      GenesisTransactionSettings("3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh", (Constants.UnitsInWave * Constants.TotalWaves * 0.02).toLong),
      GenesisTransactionSettings("3NCBMxgdghg4tUhEEffSXy11L6hUi6fcBpd", (Constants.UnitsInWave * Constants.TotalWaves * 0.02).toLong),
      GenesisTransactionSettings("3N18z4B8kyyQ96PhN5eyhCAbg4j49CgwZJx", (Constants.UnitsInWave * Constants.TotalWaves - Constants.UnitsInWave * Constants.TotalWaves * 0.1).toLong)),
    153722867L, 60.seconds)
}

case class BlockchainSettings(blockchainFile: Option[File],
                              stateFile: Option[File],
                              checkpointFile: Option[File],
                              addressSchemeCharacter: Char,
                              minimumInMemoryDiffSize: Int,
                              functionalitySettings: FunctionalitySettings,
                              genesisSettings: GenesisSettings)

object BlockchainType extends Enumeration {
  val TESTNET = Value("TESTNET")
  val MAINNET = Value("MAINNET")
  val CUSTOM = Value("CUSTOM")
}

object BlockchainSettings {
  val configPath: String = "waves.blockchain"

  def fromConfig(config: Config): BlockchainSettings = {
    val blockchainType = config.as[BlockchainType.Value](s"$configPath.type")
    val (addressSchemeCharacter, functionalitySettings, genesisSettings) = blockchainType match {
      case BlockchainType.TESTNET =>
        ('T', FunctionalitySettings.TESTNET, GenesisSettings.TESTNET)
      case BlockchainType.MAINNET =>
        ('W', FunctionalitySettings.MAINNET, GenesisSettings.MAINNET)
      case BlockchainType.CUSTOM =>
        val addressSchemeCharacter = config.as[String](s"$configPath.custom.address-scheme-character").charAt(0)
        val functionalitySettings = config.as[FunctionalitySettings]("waves.blockchain.custom.functionality")
        val genesisSettings = config.as[GenesisSettings]("waves.blockchain.custom.genesis")
        (addressSchemeCharacter, functionalitySettings, genesisSettings)
    }

    BlockchainSettings(
      blockchainFile = config.getAs[File](s"$configPath.blockchain-file"),
      stateFile = config.getAs[File](s"$configPath.state-file"),
      checkpointFile = config.getAs[File](s"$configPath.checkpoint-file"),
      addressSchemeCharacter = addressSchemeCharacter,
      minimumInMemoryDiffSize = config.as[Int](s"$configPath.minimum-in-memory-diff-blocks"),
      functionalitySettings = functionalitySettings,
      genesisSettings = genesisSettings)
  }
}

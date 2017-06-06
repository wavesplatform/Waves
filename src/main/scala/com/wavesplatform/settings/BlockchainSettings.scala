package com.wavesplatform.settings

import java.io.File

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.EnumerationReader._

import scala.collection.JavaConverters._
import scala.concurrent.duration._

case class FunctionalitySettings(allowTemporaryNegativeUntil: Long,
                                 allowInvalidPaymentTransactionsByTimestamp: Long,
                                 requireSortedTransactionsAfter: Long,
                                 generatingBalanceDepthFrom50To1000AfterHeight: Long,
                                 minimalGeneratingBalanceAfterTimestamp: Long,
                                 allowTransactionsFromFutureUntil: Long,
                                 allowUnissuedAssetsUntil: Long,
                                 allowBurnTransactionAfterTimestamp: Long,
                                 allowLeaseTransactionAfterTimestamp: Long,
                                 requirePaymentUniqueId: Long,
                                 allowExchangeTransactionAfterTimestamp: Long,
                                 allowInvalidReissueInSameBlockUntilTimestamp: Long,
                                 allowCreateAliasTransactionAfterTimestamp: Long,
                                 allowMakeAssetNameUniqueTransactionAfterTimestamp: Long,
                                 allowMultipleLeaseCancelTransactionUntilTimestamp: Long,
                                 resetEffectiveBalancesAtHeight: Long,
                                 allowLeasedBalanceTransferUntil: Long)

object FunctionalitySettings {
  val MAINNET = FunctionalitySettings(allowTemporaryNegativeUntil = 1479168000000L,
    allowInvalidPaymentTransactionsByTimestamp = 1479168000000L,
    requireSortedTransactionsAfter = 1479168000000L,
    generatingBalanceDepthFrom50To1000AfterHeight = 232000L,
    minimalGeneratingBalanceAfterTimestamp = 1479168000000L,
    allowTransactionsFromFutureUntil = 1479168000000L,
    allowUnissuedAssetsUntil = 1479416400000L,
    allowBurnTransactionAfterTimestamp = 1491192000000L,
    allowLeaseTransactionAfterTimestamp = 1491192000000L,
    requirePaymentUniqueId = 1491192000000L,
    allowExchangeTransactionAfterTimestamp = 1491192000000L,
    allowInvalidReissueInSameBlockUntilTimestamp = 1492768800000L,
    allowCreateAliasTransactionAfterTimestamp = Long.MaxValue,
    allowMakeAssetNameUniqueTransactionAfterTimestamp = Long.MaxValue,
    allowMultipleLeaseCancelTransactionUntilTimestamp = 1492768800000L,
    resetEffectiveBalancesAtHeight = 462000,
    allowLeasedBalanceTransferUntil = Long.MaxValue)

  val TESTNET = FunctionalitySettings(
    allowTemporaryNegativeUntil = 1477958400000L,
    allowInvalidPaymentTransactionsByTimestamp = 1477958400000L,
    requireSortedTransactionsAfter = 1477958400000L,
    generatingBalanceDepthFrom50To1000AfterHeight = Long.MinValue,
    minimalGeneratingBalanceAfterTimestamp = Long.MinValue,
    allowTransactionsFromFutureUntil = 1478100000000L,
    allowUnissuedAssetsUntil = 1479416400000L,
    allowBurnTransactionAfterTimestamp = 1481110521000L,
    allowLeaseTransactionAfterTimestamp = Long.MinValue,
    requirePaymentUniqueId = 1485942685000L,
    allowExchangeTransactionAfterTimestamp = 1483228800000L,
    allowInvalidReissueInSameBlockUntilTimestamp = 1492560000000L,
    allowCreateAliasTransactionAfterTimestamp = 1493596800000L,
    allowMakeAssetNameUniqueTransactionAfterTimestamp = 1495238400000L,
    allowMultipleLeaseCancelTransactionUntilTimestamp = 1492560000000L,
    resetEffectiveBalancesAtHeight = 51500,
    allowLeasedBalanceTransferUntil = 1495238400000L)

  val configPath = "waves.blockchain.custom.functionality"

  def fromConfig(config: Config): FunctionalitySettings = {
    FunctionalitySettings(allowTemporaryNegativeUntil = config.as[Long](s"$configPath.allow-temporary-negative-until"),
      allowInvalidPaymentTransactionsByTimestamp = config.as[Long](s"$configPath.allow-invalid-payment-transactions-by-timestamp"),
      requireSortedTransactionsAfter = config.as[Long](s"$configPath.require-sorted-transactions-after"),
      generatingBalanceDepthFrom50To1000AfterHeight = config.as[Long](s"$configPath.generation-balance-depth-from-50-to-1000-after-height"),
      minimalGeneratingBalanceAfterTimestamp = config.as[Long](s"$configPath.minimal-generating-balance-after"),
      allowTransactionsFromFutureUntil = config.as[Long](s"$configPath.allow-transactions-from-future-until"),
      allowUnissuedAssetsUntil = config.as[Long](s"$configPath.allow-unissued-assets-until"),
      allowBurnTransactionAfterTimestamp = config.as[Long](s"$configPath.allow-burn-transaction-after"),
      allowLeaseTransactionAfterTimestamp = config.as[Long](s"$configPath.allow-lease-transaction-after"),
      requirePaymentUniqueId = config.as[Long](s"$configPath.require-payment-unique-id-after"),
      allowExchangeTransactionAfterTimestamp = config.as[Long](s"$configPath.allow-exchange-transaction-after"),
      allowInvalidReissueInSameBlockUntilTimestamp = config.as[Long](s"$configPath.allow-invalid-reissue-in-same-block-until-timestamp"),
      allowCreateAliasTransactionAfterTimestamp = config.as[Long](s"$configPath.allow-createalias-transaction-after"),
      allowMakeAssetNameUniqueTransactionAfterTimestamp = config.as[Long](s"$configPath.allow-make-asset-name-unique-transaction-after"),
      allowMultipleLeaseCancelTransactionUntilTimestamp = config.as[Long](s"$configPath.allow-multiple-lease-cancel-transaction-until-timestamp"),
      resetEffectiveBalancesAtHeight = config.as[Long](s"$configPath.reset-effective-balances-at-height"),
      allowLeasedBalanceTransferUntil = config.as[Long](s"$configPath.allow-leased-balance-transfer-until"))
  }
}

case class GenesisTransactionSettings(recipient: String, amount: Long)

case class GenesisSettings(blockTimestamp: Long, transactionsTimestamp: Long, initialBalance: Long, signature: String,
                           transactions: List[GenesisTransactionSettings], initialBaseTarget: Long,
                           averageBlockDelay: FiniteDuration)

object GenesisSettings {
  val MAINNET = GenesisSettings(1460678400000L, 1465742577614L, Constants.UnitsInWave * Constants.TotalWaves,
    "FSH8eAAzZNqnG8xgTZtz5xuLqXySsXgAjmFEC25hXMbEufiGjqWPnGCZFt6gLiVLJny16ipxRNAkkzjjhqTjBE2",
    List(
      GenesisTransactionSettings("3PAWwWa6GbwcJaFzwqXQN5KQm7H96Y7SHTQ", Constants.UnitsInWave * Constants.TotalWaves - 5 * Constants.UnitsInWave),
      GenesisTransactionSettings("3P8JdJGYc7vaLu4UXUZc1iRLdzrkGtdCyJM", Constants.UnitsInWave),
      GenesisTransactionSettings("3PAGPDPqnGkyhcihyjMHe9v36Y4hkAh9yDy", Constants.UnitsInWave),
      GenesisTransactionSettings("3P9o3ZYwtHkaU1KxsKkFjJqJKS3dLHLC9oF", Constants.UnitsInWave),
      GenesisTransactionSettings("3PJaDyprvekvPXPuAtxrapacuDJopgJRaU3", Constants.UnitsInWave),
      GenesisTransactionSettings("3PBWXDFUc86N2EQxKJmW8eFco65xTyMZx6J", Constants.UnitsInWave)),
    153722867L, 60.seconds)

  val TESTNET = GenesisSettings(1460678400000L, 1478000000000L, Constants.UnitsInWave * Constants.TotalWaves,
    "5uqnLK3Z9eiot6FyYBfwUnbyid3abicQbAZjz38GQ1Q8XigQMxTK4C1zNkqS1SVw7FqSidbZKxWAKLVoEsp4nNqa",
    List(
      GenesisTransactionSettings("3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8", (Constants.UnitsInWave * Constants.TotalWaves * 0.04).toLong),
      GenesisTransactionSettings("3NBVqYXrapgJP9atQccdBPAgJPwHDKkh6A8", (Constants.UnitsInWave * Constants.TotalWaves * 0.02).toLong),
      GenesisTransactionSettings("3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh", (Constants.UnitsInWave * Constants.TotalWaves * 0.02).toLong),
      GenesisTransactionSettings("3NCBMxgdghg4tUhEEffSXy11L6hUi6fcBpd", (Constants.UnitsInWave * Constants.TotalWaves * 0.02).toLong),
      GenesisTransactionSettings("3N18z4B8kyyQ96PhN5eyhCAbg4j49CgwZJx", (Constants.UnitsInWave * Constants.TotalWaves - Constants.UnitsInWave * Constants.TotalWaves * 0.1).toLong)),
    153722867L, 60.seconds)

  val configPath: String = "waves.blockchain.custom.genesis"

  def fromConfig(config: Config): GenesisSettings = {
    val timestamp = config.as[Long](s"$configPath.timestamp")
    val initialBalance = config.as[Long](s"$configPath.initial-balance")
    val signature = config.as[String](s"$configPath.signature")
    val transactions = config.getConfigList(s"$configPath.transactions").asScala.map { p: Config =>
      GenesisTransactionSettings(p.as[String]("recipient"), p.as[Long]("amount"))
    }.toList
    val initialBaseTarget = config.as[Long](s"$configPath.initial-base-target")
    val averageBlockDelay = config.as[FiniteDuration](s"$configPath.average-block-delay")

    GenesisSettings(timestamp, timestamp, initialBalance, signature, transactions, initialBaseTarget, averageBlockDelay)
  }
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
        val functionalitySettings = FunctionalitySettings.fromConfig(config)
        val genesisSettings = GenesisSettings.fromConfig(config)
        (addressSchemeCharacter, functionalitySettings, genesisSettings)
    }

    BlockchainSettings(
      blockchainFile = config.as[Option[File]](s"$configPath.blockchain-file"),
      stateFile = config.as[Option[File]](s"$configPath.state-file"),
      checkpointFile = config.as[Option[File]](s"$configPath.checkpoint-file"),
      addressSchemeCharacter = addressSchemeCharacter,
      minimumInMemoryDiffSize = config.as[Int](s"$configPath.minimum-in-memory-diff-blocks"),
      functionalitySettings = functionalitySettings,
      genesisSettings = genesisSettings)
  }
}

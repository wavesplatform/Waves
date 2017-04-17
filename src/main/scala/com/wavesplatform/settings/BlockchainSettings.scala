package com.wavesplatform.settings

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.EnumerationReader._
import scorex.account.{Account, AddressScheme}
import scorex.settings.ChainParameters
import scorex.transaction.GenesisTransaction

import scala.collection.JavaConverters._

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
                                 allowMultipleLeaseCancelTransactionUntilTimestamp: Long,
                                 resetEffectiveBalancesAtHeight: Long)

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
    allowInvalidReissueInSameBlockUntilTimestamp = 1492682400000L,
    allowMultipleLeaseCancelTransactionUntilTimestamp = 1492682400000L,
    resetEffectiveBalancesAtHeight = 460400)

  val TESTNET = FunctionalitySettings(
    allowTemporaryNegativeUntil = 1477958400000L,
    allowInvalidPaymentTransactionsByTimestamp = 1477958400000L,
    requireSortedTransactionsAfter = 1477958400000L,
    generatingBalanceDepthFrom50To1000AfterHeight = Long.MinValue,
    minimalGeneratingBalanceAfterTimestamp = Long.MinValue,
    allowTransactionsFromFutureUntil = Long.MinValue,
    allowUnissuedAssetsUntil = 1479416400000L,
    allowBurnTransactionAfterTimestamp = 1481110521000L,
    allowLeaseTransactionAfterTimestamp = Long.MinValue,
    requirePaymentUniqueId = 1485942685000L,
    allowExchangeTransactionAfterTimestamp = 1483228800000L,
    allowInvalidReissueInSameBlockUntilTimestamp = 1492387200000L,
    allowMultipleLeaseCancelTransactionUntilTimestamp = 1492387200000L,
    resetEffectiveBalancesAtHeight = 49500)

  val configPath = "waves.blockchain.custom.functionality"

  def fromConfig(config: Config): FunctionalitySettings = {
    val allowTemporaryNegativeUntil = config.as[Long](s"$configPath.allow-temporary-negative-until")
    val allowInvalidPaymentTransactionsByTimestamp = config.as[Long](s"$configPath.allow-invalid-payment-transactions-by-timestamp")
    val requireSortedTransactionsAfter = config.as[Long](s"$configPath.require-sorted-transactions-after")
    val generatingBalanceDepthFrom50To1000AfterHeight = config.as[Long](s"$configPath.generation-balance-depth-from-50-to-1000-after-height")
    val minimalGeneratingBalanceAfterTimestamp = config.as[Long](s"$configPath.minimal-generating-balance-after")
    val allowTransactionsFromFutureUntil = config.as[Long](s"$configPath.allow-transactions-from-future-until")
    val allowUnissuedAssetsUntil = config.as[Long](s"$configPath.allow-unissued-assets-until")
    val allowBurnTransactionAfterTimestamp = config.as[Long](s"$configPath.allow-burn-transaction-after")
    val allowLeaseTransactionAfterTimestamp = config.as[Long](s"$configPath.allow-lease-transaction-after")
    val requirePaymentUniqueId = config.as[Long](s"$configPath.require-payment-unique-id-after")
    val allowExchangeTransactionAfterTimestamp = config.as[Long](s"$configPath.allow-exchange-transaction-after")
    val allowInvalidReissueInSameBlockUntilTimestamp = config.as[Long](s"$configPath.allow-invalid-reissue-in-same-block-until-timestamp")
    val allowMultipleLeaseCancelTransactionUntilTimestamp = config.as[Long](s"$configPath.allow-multiple-lease-cancel-transaction-until-timestamp")
    val resetEffectiveBalancesAfterTimestamp = config.as[Long](s"$configPath.reset-effective-balances-at-height")

    FunctionalitySettings(allowTemporaryNegativeUntil, allowInvalidPaymentTransactionsByTimestamp,
      requireSortedTransactionsAfter, generatingBalanceDepthFrom50To1000AfterHeight,
      minimalGeneratingBalanceAfterTimestamp, allowTransactionsFromFutureUntil, allowUnissuedAssetsUntil,
      allowBurnTransactionAfterTimestamp, allowLeaseTransactionAfterTimestamp, requirePaymentUniqueId, allowExchangeTransactionAfterTimestamp,allowInvalidReissueInSameBlockUntilTimestamp,allowMultipleLeaseCancelTransactionUntilTimestamp, resetEffectiveBalancesAfterTimestamp)
  }
}

case class GenesisTransactionSettings(recipient: String, amount: Long)

case class GenesisSettings(timestamp: Long, initialBalance: Long, signature: String,
                           transactions: List[GenesisTransactionSettings])

object GenesisSettings {
  val MAINNET = GenesisSettings(1465742577614L, Constants.UnitsInWave * Constants.TotalWaves,
    "FSH8eAAzZNqnG8xgTZtz5xuLqXySsXgAjmFEC25hXMbEufiGjqWPnGCZFt6gLiVLJny16ipxRNAkkzjjhqTjBE2",
    List(
      GenesisTransactionSettings("3PAWwWa6GbwcJaFzwqXQN5KQm7H96Y7SHTQ", Constants.UnitsInWave * Constants.TotalWaves - 5 * Constants.UnitsInWave),
      GenesisTransactionSettings("3P8JdJGYc7vaLu4UXUZc1iRLdzrkGtdCyJM", Constants.UnitsInWave),
      GenesisTransactionSettings("3PAGPDPqnGkyhcihyjMHe9v36Y4hkAh9yDy", Constants.UnitsInWave),
      GenesisTransactionSettings("3P9o3ZYwtHkaU1KxsKkFjJqJKS3dLHLC9oF", Constants.UnitsInWave),
      GenesisTransactionSettings("3PJaDyprvekvPXPuAtxrapacuDJopgJRaU3", Constants.UnitsInWave),
      GenesisTransactionSettings("3PBWXDFUc86N2EQxKJmW8eFco65xTyMZx6J", Constants.UnitsInWave)))

  val TESTNET = GenesisSettings(1478000000000L, Constants.UnitsInWave * Constants.TotalWaves,
    "5uqnLK3Z9eiot6FyYBfwUnbyid3abicQbAZjz38GQ1Q8XigQMxTK4C1zNkqS1SVw7FqSidbZKxWAKLVoEsp4nNqa",
    List(
      GenesisTransactionSettings("3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8", (Constants.UnitsInWave * Constants.TotalWaves * 0.04).toLong),
      GenesisTransactionSettings("3NBVqYXrapgJP9atQccdBPAgJPwHDKkh6A8", (Constants.UnitsInWave * Constants.TotalWaves * 0.02).toLong),
      GenesisTransactionSettings("3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh", (Constants.UnitsInWave * Constants.TotalWaves * 0.02).toLong),
      GenesisTransactionSettings("3NCBMxgdghg4tUhEEffSXy11L6hUi6fcBpd", (Constants.UnitsInWave * Constants.TotalWaves * 0.02).toLong),
      GenesisTransactionSettings("3N18z4B8kyyQ96PhN5eyhCAbg4j49CgwZJx", (Constants.UnitsInWave * Constants.TotalWaves - Constants.UnitsInWave * Constants.TotalWaves * 0.1).toLong)))

  val configPath: String = "waves.blockchain.custom.genesis"

  def fromConfig(config: Config): GenesisSettings = {
    val timestamp = config.as[Long](s"$configPath.timestamp")
    val initialBalance = config.as[Long](s"$configPath.initial-balance")
    val signature = config.as[String](s"$configPath.signature")
    val transactions = config.getConfigList(s"$configPath.transactions").asScala.map { p: Config =>
      GenesisTransactionSettings(p.as[String]("recipient"), p.as[Long]("amount"))
    }.toList

    GenesisSettings(timestamp, initialBalance, signature, transactions)
  }
}

case class BlockchainSettings(file: String,
                              addressSchemeCharacter: Char,
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
    val file = config.as[String](s"$configPath.file")

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

    BlockchainSettings(file, addressSchemeCharacter, functionalitySettings, genesisSettings)
  }
}

object BlockchainSettingsExtension {

  implicit class BackwardCompatibleBlockchainSettings(val blockchainSettings: BlockchainSettings) extends AnyVal {
    def asChainParameters: ChainParameters = {
      new ChainParameters {
        override def addressScheme = new AddressScheme {
          override val chainId: Byte = blockchainSettings.addressSchemeCharacter.toByte
        }

        override def genesisTxs: Seq[GenesisTransaction] =
          buildTransactions(blockchainSettings.genesisSettings.transactions)

        override def genesisTimestamp: Long = blockchainSettings.genesisSettings.timestamp

        override def initialBalance: Long = blockchainSettings.genesisSettings.initialBalance

        override def allowTemporaryNegativeUntil: Long =
          blockchainSettings.functionalitySettings.allowTemporaryNegativeUntil

        override def requirePaymentUniqueId: Long = blockchainSettings.functionalitySettings.requirePaymentUniqueId

        override def minimalGeneratingBalanceAfterTimestamp: Long =
          blockchainSettings.functionalitySettings.minimalGeneratingBalanceAfterTimestamp

        override def generatingBalanceDepthFrom50To1000AfterHeight: Long =
          blockchainSettings.functionalitySettings.generatingBalanceDepthFrom50To1000AfterHeight

        override def requireSortedTransactionsAfter: Long =
          blockchainSettings.functionalitySettings.requireSortedTransactionsAfter

        override def allowTransactionsFromFutureUntil: Long =
          blockchainSettings.functionalitySettings.allowTransactionsFromFutureUntil

        override def allowBurnTransactionAfterTimestamp: Long =
          blockchainSettings.functionalitySettings.allowBurnTransactionAfterTimestamp

        override def allowInvalidPaymentTransactionsByTimestamp: Long =
          blockchainSettings.functionalitySettings.allowInvalidPaymentTransactionsByTimestamp

        override def allowUnissuedAssetsUntil: Long = blockchainSettings.functionalitySettings.allowUnissuedAssetsUntil

        override def allowLeaseTransactionAfterTimestamp: Long = blockchainSettings.functionalitySettings.allowLeaseTransactionAfterTimestamp

        override def allowExchangeTransactionAfterTimestamp: Long = blockchainSettings.functionalitySettings.allowExchangeTransactionAfterTimestamp

        override def allowInvalidReissueInSameBlockUntilTimestamp: Long = blockchainSettings.functionalitySettings.allowInvalidReissueInSameBlockUntilTimestamp

        override def allowMultipleLeaseCancelTransactionUntilTimestamp: Long = blockchainSettings.functionalitySettings.allowMultipleLeaseCancelTransactionUntilTimestamp

        override def resetEffectiveBalancesAtHeight: Long = blockchainSettings.functionalitySettings.resetEffectiveBalancesAtHeight
      }
    }

    private def buildTransactions(transactionSettings: List[GenesisTransactionSettings]): Seq[GenesisTransaction] = {
      transactionSettings.map { ts =>
        GenesisTransaction.create(new Account(ts.recipient), ts.amount, blockchainSettings.genesisSettings.timestamp)
          .right.get
      }
    }
  }
}

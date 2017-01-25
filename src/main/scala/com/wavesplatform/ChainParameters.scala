package com.wavesplatform

import scorex.account.{Account, AddressScheme}
import scorex.transaction.{GenesisTransaction, Transaction}
import com.wavesplatform.settings.Constants
import scorex.settings.WavesHardForkParameters

/**
  * ChainParameters contains the data needed for working with an instantiation of particular chain
  */
abstract class ChainParameters extends WavesHardForkParameters {
  val initialBalance: Long
  val genesisTimestamp: Long
  val genesisTxs: Seq[Transaction]
  val addressScheme: AddressScheme
}

object TestNetParams extends ChainParameters {
  val initialBalance: Long = Constants.UnitsInWave * Constants.TotalWaves
  val genesisTimestamp = 1478000000000L
  val singleNodeBalance: Double = initialBalance * 0.02
  lazy val genesisTxs: Seq[GenesisTransaction] = {
    val txs = Seq(
      GenesisTransaction.create(new Account("3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8"), (2 * singleNodeBalance).toLong, genesisTimestamp).right.get,
      GenesisTransaction.create(new Account("3NBVqYXrapgJP9atQccdBPAgJPwHDKkh6A8"), singleNodeBalance.toLong, genesisTimestamp).right.get,
      GenesisTransaction.create(new Account("3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh"), singleNodeBalance.toLong, genesisTimestamp).right.get,
      GenesisTransaction.create(new Account("3NCBMxgdghg4tUhEEffSXy11L6hUi6fcBpd"), singleNodeBalance.toLong, genesisTimestamp).right.get,
      GenesisTransaction.create(new Account("3N18z4B8kyyQ96PhN5eyhCAbg4j49CgwZJx"), (initialBalance - 5 * singleNodeBalance).toLong, genesisTimestamp).right.get
    )
    require(txs.foldLeft(0L)(_ + _.amount) == initialBalance)
    txs
  }
  override val addressScheme: AddressScheme = new AddressScheme {
    override val chainId: Byte = 'T'.toByte
  }

  override val allowTemporaryNegativeUntil: Long = 1477958400000L

  override val requireSortedTransactionsAfter: Long = 1477958400000L

  override val allowInvalidPaymentTransactionsByTimestamp: Long = 1477958400000L

  override val generatingBalanceDepthFrom50To1000AfterHeight: Long = Long.MinValue

  override val minimalGeneratingBalanceAfterTimestamp: Long = Long.MinValue

  override val allowTransactionsFromFutureUntil: Long = Long.MinValue

  override val allowUnissuedAssetsUntil: Long = 1479416400000L

  override val allowBurnTransactionAfterTimestamp: Long = 1481110521000L

  override val requirePaymentUniqueId: Long = 1485942685000L
}

object MainNetParams extends ChainParameters {
  val initialBalance: Long = Constants.UnitsInWave * Constants.TotalWaves
  val genesisTimestamp = 1465742577614L
  lazy val genesisTxs: Seq[GenesisTransaction] = {
    val txs = Seq(
      GenesisTransaction.create(new Account("3PAWwWa6GbwcJaFzwqXQN5KQm7H96Y7SHTQ"), initialBalance - 5 * Constants.UnitsInWave, genesisTimestamp).right.get,
      GenesisTransaction.create(new Account("3P8JdJGYc7vaLu4UXUZc1iRLdzrkGtdCyJM"), Constants.UnitsInWave, genesisTimestamp).right.get,
      GenesisTransaction.create(new Account("3PAGPDPqnGkyhcihyjMHe9v36Y4hkAh9yDy"), Constants.UnitsInWave, genesisTimestamp).right.get,
      GenesisTransaction.create(new Account("3P9o3ZYwtHkaU1KxsKkFjJqJKS3dLHLC9oF"), Constants.UnitsInWave, genesisTimestamp).right.get,
      GenesisTransaction.create(new Account("3PJaDyprvekvPXPuAtxrapacuDJopgJRaU3"), Constants.UnitsInWave, genesisTimestamp).right.get,
      GenesisTransaction.create(new Account("3PBWXDFUc86N2EQxKJmW8eFco65xTyMZx6J"), Constants.UnitsInWave, genesisTimestamp).right.get
    )
    require(txs.foldLeft(0L)(_ + _.amount) == initialBalance)
    txs
  }
  override val addressScheme: AddressScheme = new AddressScheme {
    override val chainId: Byte = 'W'.toByte
  }
  override val allowTemporaryNegativeUntil: Long = 1479168000000L

  override val requireSortedTransactionsAfter: Long = 1479168000000L

  override val allowInvalidPaymentTransactionsByTimestamp: Long = 1479168000000L

  override val generatingBalanceDepthFrom50To1000AfterHeight: Long = 232000L

  override val minimalGeneratingBalanceAfterTimestamp: Long = 1479168000000L

  override val allowTransactionsFromFutureUntil: Long = 1479168000000L

  override val allowUnissuedAssetsUntil: Long = 1479416400000L

  override val allowBurnTransactionAfterTimestamp: Long = 1482233593000L

  override val requirePaymentUniqueId: Long = 1488361885000L
}
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
  val genesisTxs : Seq[Transaction]
  val addressScheme: AddressScheme
}

object TestNetParams extends ChainParameters {
  val initialBalance = Constants.UnitsInWave * Constants.TotalWaves
  val genesisTimestamp = 1478000000000L
  val singleNodeBalance = initialBalance * 0.02
  val genesisTxs = {
    val txs = Seq(
      GenesisTransaction.create(new Account("3MZhiaZx7L2PcUFr1tr6ZdxrLEmorPZmgpz"), (2 * singleNodeBalance).toLong, genesisTimestamp).right.get,
      GenesisTransaction.create(new Account("3MnAEZRZJ7VqWL9kKbCHru3QWmS2GheDkCz"), singleNodeBalance.toLong, genesisTimestamp).right.get,
      GenesisTransaction.create(new Account("3MfvprsutzZ2exmdy3BEy8rkVB4i9711J9S"), singleNodeBalance.toLong, genesisTimestamp).right.get,
      GenesisTransaction.create(new Account("3MnqkyaLPzVc1fG69eF7DUsjYUCDmPPneAa"), singleNodeBalance.toLong, genesisTimestamp).right.get,
      GenesisTransaction.create(new Account("3MboP54qUGnwGGxZH4EeNi3KtSDoCRJwC3W"), (initialBalance - 5 * singleNodeBalance).toLong, genesisTimestamp).right.get
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
}

object MainNetParams extends ChainParameters {
  val initialBalance = Constants.UnitsInWave * Constants.TotalWaves
  val genesisTimestamp = 1465742577614L
  val genesisTxs = {
    val txs = Seq(
      GenesisTransaction.create( new Account("3PAWwWa6GbwcJaFzwqXQN5KQm7H96Y7SHTQ"), initialBalance - 5 * Constants.UnitsInWave, genesisTimestamp).right.get,
      GenesisTransaction.create( new Account("3P8JdJGYc7vaLu4UXUZc1iRLdzrkGtdCyJM"), Constants.UnitsInWave, genesisTimestamp).right.get,
      GenesisTransaction.create( new Account("3PAGPDPqnGkyhcihyjMHe9v36Y4hkAh9yDy"), Constants.UnitsInWave, genesisTimestamp).right.get,
      GenesisTransaction.create( new Account("3P9o3ZYwtHkaU1KxsKkFjJqJKS3dLHLC9oF"), Constants.UnitsInWave, genesisTimestamp).right.get,
      GenesisTransaction.create( new Account("3PJaDyprvekvPXPuAtxrapacuDJopgJRaU3"), Constants.UnitsInWave, genesisTimestamp).right.get,
      GenesisTransaction.create( new Account("3MyViFvajzYyPn7Y4EWWBBsoSCaBdrCZSfw"), Constants.UnitsInWave, genesisTimestamp).right.get
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
}

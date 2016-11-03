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
  val genesisTimestamp = 1460952000000L
  val genesisTxs = {
    val txs = Seq(
      GenesisTransaction(new Account("3N2AS1sN9HztLv8s7KACbPxKu669qUYMQDv"), (initialBalance * 0.04).toLong, genesisTimestamp),
      GenesisTransaction(new Account("3MrVPwM5Hqexu89smSfS1csm8nBwh6E17P4"), (initialBalance * 0.02).toLong, genesisTimestamp),
      GenesisTransaction(new Account("3Ms8KmugMcKjAZ2t8WxWDXXvyMyuCZtqj9x"), (initialBalance * 0.02).toLong, genesisTimestamp),
      GenesisTransaction(new Account("3MuXt4mDCrdohta3Z5vyYQVZutJ8bghJTmC"), (initialBalance * 0.02).toLong, genesisTimestamp),
      GenesisTransaction(new Account("3Mrk1WipRAaX3sBs8aqtmSpKzAdYBoKZ1Zq"), (initialBalance * 0.9).toLong, genesisTimestamp)
    )
    require(txs.foldLeft(0L)(_ + _.amount) == initialBalance)
    txs
  }
  override val addressScheme: AddressScheme = new AddressScheme {
    override val chainId: Byte = 'T'.toByte
  }

  override def allowTemporaryNegativeUntil: Long = 1477958400000L

  override def requireSortedTransactionsAfter: Long = 1477958400000L
}

object MainNetParams extends ChainParameters {
  val initialBalance = Constants.UnitsInWave * Constants.TotalWaves
  val genesisTimestamp = 1460952000000L
  val genesisTxs = {
    val txs = Seq(
      GenesisTransaction( new Account("3PAWwWa6GbwcJaFzwqXQN5KQm7H96Y7SHTQ"), initialBalance - 5 * Constants.UnitsInWave, genesisTimestamp),
      GenesisTransaction( new Account("3P8JdJGYc7vaLu4UXUZc1iRLdzrkGtdCyJM"), Constants.UnitsInWave, genesisTimestamp),
      GenesisTransaction( new Account("3PAGPDPqnGkyhcihyjMHe9v36Y4hkAh9yDy"), Constants.UnitsInWave, genesisTimestamp),
      GenesisTransaction( new Account("3P9o3ZYwtHkaU1KxsKkFjJqJKS3dLHLC9oF"), Constants.UnitsInWave, genesisTimestamp),
      GenesisTransaction( new Account("3PJaDyprvekvPXPuAtxrapacuDJopgJRaU3"), Constants.UnitsInWave, genesisTimestamp),
      GenesisTransaction( new Account("3PBWXDFUc86N2EQxKJmW8eFco65xTyMZx6J"), Constants.UnitsInWave, genesisTimestamp)
    )
    require(txs.foldLeft(0L)(_ + _.amount) == initialBalance)
    txs
  }
  override val addressScheme: AddressScheme = new AddressScheme {
    override val chainId: Byte = 'W'.toByte
  }
  override def allowTemporaryNegativeUntil: Long = 1478736000000L

  override def requireSortedTransactionsAfter: Long = 1478736000000L
}

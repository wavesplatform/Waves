package com.wavesplatform

import scorex.account.{Account, AddressScheme}
import scorex.transaction.{GenesisTransaction, Transaction}
import com.wavesplatform.settings.Constants

/**
  * ChainParameters contains the data needed for working with an instantiation of particular chain
  */
abstract class ChainParameters {
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
      GenesisTransaction(new Account("3N5jhcA7R98AUN12ee9pB7unvnAKfzb3nen"), initialBalance - 5 * Constants.UnitsInWave, genesisTimestamp),
      GenesisTransaction(new Account("3MyTvqfeLWkvjSZ1hwkhQjzipZr7Pk8dyMR"), Constants.UnitsInWave, genesisTimestamp),
      GenesisTransaction(new Account("3MqS3mVY4Yr4HoTdpWiEaq9phwbaoWS2W6A"), Constants.UnitsInWave, genesisTimestamp),
      GenesisTransaction(new Account("3N3CDuzGXB2qP5vb2NvnnDQ68HahNCfYVBg"), Constants.UnitsInWave, genesisTimestamp),
      GenesisTransaction(new Account("3N2sacZ9XTQUkLDdZZgtb1zJUAmr6oziRrU"), Constants.UnitsInWave, genesisTimestamp),
      GenesisTransaction(new Account("3N189PMB8BaxngN3fNvDRkFbvbH8xMkk328"), Constants.UnitsInWave, genesisTimestamp)
    )
    require(txs.foldLeft(0L)(_ + _.amount) == initialBalance)
    txs
  }
  override val addressScheme: AddressScheme = new AddressScheme {
    override val chainId: Byte = 'T'.toByte
  }
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
}
package com.wavesplatform.features

case class BlockchainFeature private(id: Short)

object BlockchainFeatures {

  val SmallerMinimalGeneratingBalance = BlockchainFeature(1)
  val NG = BlockchainFeature(2)
  val MassTransfer = BlockchainFeature(3)
  val DataTransaction = BlockchainFeature(4)
  val SmartAccounts = BlockchainFeature(5)

  val implemented: Set[Short] = Set(SmallerMinimalGeneratingBalance, NG, MassTransfer, DataTransaction, SmartAccounts).map(_.id)

}

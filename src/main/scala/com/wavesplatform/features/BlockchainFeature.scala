package com.wavesplatform.features

case class BlockchainFeature private(id: Short, description: String)

object BlockchainFeatures {

  val SmallerMinimalGeneratingBalance = BlockchainFeature(1, "Minimum Generating Balance of 1000 WAVES")
  val NG = BlockchainFeature(2, "NG Protocol")
  val MassTransfer = BlockchainFeature(3, "Mass Transfer Transaction")
  val SmartAccounts = BlockchainFeature(4, "Smart Accounts")
  val DataTransaction = BlockchainFeature(5, "Data Transaction")

  private val dict = Seq(SmallerMinimalGeneratingBalance, NG, MassTransfer, SmartAccounts, DataTransaction)
    .map(f => f.id -> f)
    .toMap

  val implemented: Set[Short] = dict.keySet

  def feature(id: Short) = dict(id)
}

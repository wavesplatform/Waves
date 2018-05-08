package com.wavesplatform.features

case class BlockchainFeature private (id: Short, description: String)

object BlockchainFeatures {

  val SmallerMinimalGeneratingBalance = BlockchainFeature(1, "Minimum Generating Balance of 1000 WAVES")
  val NG                              = BlockchainFeature(2, "NG Protocol")
  val MassTransfer                    = BlockchainFeature(3, "Mass Transfer Transaction")
  val SmartAccounts                   = BlockchainFeature(4, "Smart Accounts")
  val DataTransaction                 = BlockchainFeature(5, "Data Transaction")
  val BurnAnyTokens                   = BlockchainFeature(6, "Burn Any Tokens")
  val FeeSponsorship                  = BlockchainFeature(7, "Fee Sponsorship")
  val DataTransactionV2               = BlockchainFeature(8, "Data Transaction V2")

  private val dict = Seq(
    SmallerMinimalGeneratingBalance,
    NG,
    MassTransfer,
    SmartAccounts,
    DataTransaction,
    DataTransactionV2,
    BurnAnyTokens,
    FeeSponsorship
  ).map(f => f.id -> f).toMap

  val implemented: Set[Short] = dict.keySet

  def feature(id: Short) = dict(id)
}

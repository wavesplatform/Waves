package com.wavesplatform.features

case class BlockchainFeature(id: Short, description: String)

object BlockchainFeatures {

  val SmallerMinimalGeneratingBalance = BlockchainFeature(1, "Minimum Generating Balance of 1000 WAVES")
  val NG                              = BlockchainFeature(2, "NG Protocol")
  val MassTransfer                    = BlockchainFeature(3, "Mass Transfer Transaction")
  val SmartAccounts                   = BlockchainFeature(4, "Smart Accounts")
  val DataTransaction                 = BlockchainFeature(5, "Data Transaction")
  val BurnAnyTokens                   = BlockchainFeature(6, "Burn Any Tokens")
  val FeeSponsorship                  = BlockchainFeature(7, "Fee Sponsorship")
  val FairPoS                         = BlockchainFeature(8, "Fair PoS")
  val SmartAssets                     = BlockchainFeature(9, "Smart Assets")
  val SmartAccountTrading             = BlockchainFeature(10, "Smart Account Trading")
  val Ride4DApps                      = BlockchainFeature(11, "RIDE 4 DAPPS")
  val OrderV3                         = BlockchainFeature(12, "Order Version 3")
  val ReduceNFTFee                    = BlockchainFeature(13, "Reduce NFT fee")
  val BlockReward                     = BlockchainFeature(14, "Block Reward and Community Driven Monetary Policy")
  val BlockV5                         = BlockchainFeature(15, "Ride V4, VRF, Protobuf, Failed transactions")
  val SynchronousCalls                = BlockchainFeature(16, "Ride V5, dApp-to-dApp invocations")
  val RideV6                          = BlockchainFeature(17, "Ride V6, MetaMask support")
  val ConsensusImprovements           = BlockchainFeature(18, "Consensus and MetaMask updates")
  val BlockRewardDistribution         = BlockchainFeature(19, "Block Reward Distribution")
  val CappedReward                    = BlockchainFeature(20, "Capped XTN buy-back & DAO amounts")
  val CeaseXtnBuyback                 = BlockchainFeature(21, "Cease XTN buy-back")
  val LightNode                       = BlockchainFeature(22, "Light Node")
  val BoostBlockReward                = BlockchainFeature(23, "Boost Block Reward")

  // Not exposed
  val ContinuationTransaction = BlockchainFeature(24, "Continuation Transaction")
  val LeaseExpiration         = BlockchainFeature(25, "Lease Expiration")

  // When next fork-parameter is created, you must replace all uses of the DummyFeature with the new one.
  val Dummy = BlockchainFeature(-1, "Non Votable!")

  private val dict = Seq(
    SmallerMinimalGeneratingBalance,
    NG,
    MassTransfer,
    SmartAccounts,
    DataTransaction,
    BurnAnyTokens,
    FeeSponsorship,
    FairPoS,
    SmartAccountTrading,
    SmartAssets,
    Ride4DApps,
    OrderV3,
    ReduceNFTFee,
    BlockReward,
    BlockV5,
    SynchronousCalls,
    RideV6,
    ConsensusImprovements,
    BlockRewardDistribution,
    CappedReward,
    CeaseXtnBuyback,
    LightNode,
    BoostBlockReward
  ).map(f => f.id -> f).toMap

  val implemented: Set[Short] = dict.keySet

  def feature(id: Short): Option[BlockchainFeature] = dict.get(id)
}

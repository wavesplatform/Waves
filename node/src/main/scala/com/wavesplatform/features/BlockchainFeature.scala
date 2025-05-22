package com.wavesplatform.features

case class BlockchainFeature(id: Short, description: String)

enum BF(val id: Short, val description: String, val ready: Boolean = true) {
  case SmallerMinimalGeneratingBalance extends BF(1, "Minimum Generating Balance of 1000 WAVES")
  case NG                              extends BF(2, "NG Protocol")
  case MassTransfer                    extends BF(3, "Mass Transfer Transaction")
  case SmartAccounts                   extends BF(4, "Smart Accounts")
  case DataTransaction                 extends BF(5, "Data Transaction")
  case BurnAnyTokens                   extends BF(6, "Burn Any Tokens")
  case FeeSponsorship                  extends BF(7, "Fee Sponsorship")
  case FairPoS                         extends BF(8, "Fair PoS")
  case SmartAssets                     extends BF(9, "Smart Assets")
  case SmartAccountTrading             extends BF(10, "Smart Account Trading")
  case Ride4DApps                      extends BF(11, "RIDE 4 DAPPS")
  case OrderV3                         extends BF(12, "Order Version 3")
  case ReduceNFTFee                    extends BF(13, "Reduce NFT fee")
  case BlockReward                     extends BF(14, "Block Reward and Community Driven Monetary Policy")
  case BlockV5                         extends BF(15, "Ride V4, VRF, Protobuf, Failed transactions")
  case SynchronousCalls                extends BF(16, "Ride V5, dApp-to-dApp invocations")
  case RideV6                          extends BF(17, "Ride V6, MetaMask support")
  case ConsensusImprovements           extends BF(18, "Consensus and MetaMask updates")
  case BlockRewardDistribution         extends BF(19, "Block Reward Distribution")
  case CappedReward                    extends BF(20, "Capped XTN buy-back & DAO amounts")
  case CeaseXtnBuyback                 extends BF(21, "Cease XTN buy-back")
  case LightNode                       extends BF(22, "Light Node")
  case BoostBlockReward                extends BF(23, "Boost Block Reward")
  case EcrecoverFix                    extends BF(24, "ecrecover fix")
  case Finality                        extends BF(25, "Finality")

  // Not exposed
  case ContinuationTransaction extends BF(26, "Continuation Transaction", ready = false)
  case LeaseExpiration         extends BF(27, "Lease Expiration", ready = false)

  // When next fork-parameter is created, you must replace all uses of the DummyFeature with the new one.
  case Dummy extends BF(-1, "Non Votable!", ready = false)
}

object BF {
  val implemented: Set[Short] = BF.values.collect { case bf if bf.ready => bf.id }.toSet
}

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
  val EcrecoverFix                    = BlockchainFeature(24, "ecrecover fix")

  // Not exposed
  val ContinuationTransaction = BlockchainFeature(25, "Continuation Transaction")
  val LeaseExpiration         = BlockchainFeature(26, "Lease Expiration")

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
    BoostBlockReward,
    EcrecoverFix
  ).map(f => f.id -> f).toMap

  val implemented: Set[Short] = dict.keySet

  def feature(id: Short): Option[BlockchainFeature] = dict.get(id)
}

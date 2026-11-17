package com.wavesplatform.state

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.settings.Constants
import com.wavesplatform.state.diffs.BlockDiffer.Fraction

object BlockRewardCalculator {

  case class BlockRewardShares(miner: Long, daoAddress: Long, xtnBuybackAddress: Long) {
    private[BlockRewardCalculator] def multiply(by: Long): BlockRewardShares = BlockRewardShares(
      miner = miner * by,
      daoAddress = daoAddress * by,
      xtnBuybackAddress = xtnBuybackAddress * by
    )
  }

  val CurrentBlockRewardPart: Fraction   = Fraction(1, 3)
  val RemaindRewardAddressPart: Fraction = Fraction(1, 2)

  val FullRewardInit: Long        = 6 * Constants.UnitsInWave
  val MaxAddressReward: Long      = 2 * Constants.UnitsInWave
  val GuaranteedMinerReward: Long = 2 * Constants.UnitsInWave
  val RewardBoost                 = 10

  /** How the block reward is split between the miner, the DAO address and the XTN buyback address once
    * [[BlockchainFeatures.CappedReward]] is activated.
    *
    * @param fullReward
    *   the block reward at which both the DAO and the XTN buyback addresses receive their maximum shares
    * @param guaranteedMinerReward
    *   the part of the block reward the miner receives regardless of the votes
    * @param maxDaoAddressReward
    *   the maximum share of the DAO address
    * @param maxXtnBuybackAddressReward
    *   the maximum share of the XTN buyback address
    * @param daoAddressRemainderPart
    *   the part of a below-[[fullReward]] block reward left after the guaranteed miner reward that goes to the DAO address
    * @param xtnBuybackAddressRemainderPart
    *   the same for the XTN buyback address
    */
  case class RewardDistribution(
      fullReward: Long,
      guaranteedMinerReward: Long,
      maxDaoAddressReward: Long,
      maxXtnBuybackAddressReward: Long,
      daoAddressRemainderPart: Fraction,
      xtnBuybackAddressRemainderPart: Fraction
  )

  /** 2 (DAO) / 2 (miner) / 2 (XTN buyback) at the initial block reward of 6 waves. */
  val DefaultDistribution: RewardDistribution = RewardDistribution(
    FullRewardInit,
    GuaranteedMinerReward,
    MaxAddressReward,
    MaxAddressReward,
    RemaindRewardAddressPart,
    RemaindRewardAddressPart
  )

  /** 10 (DAO) / 8 (miner) / 2 (XTN buyback) at the block reward of 20 waves, used after the activation of
    * [[BlockchainFeatures.AdjustedBlockRewardDistribution]]. 20 waves is the amount the block reward is reset to at the activation height, it stays
    * votable afterwards.
    */
  val AdjustedFullReward: Long              = 20 * Constants.UnitsInWave
  val AdjustedDaoAddressReward: Long        = 10 * Constants.UnitsInWave
  val AdjustedXtnBuybackAddressReward: Long = 2 * Constants.UnitsInWave
  val AdjustedGuaranteedMinerReward: Long   = AdjustedFullReward - AdjustedDaoAddressReward - AdjustedXtnBuybackAddressReward

  val AdjustedDistribution: RewardDistribution = RewardDistribution(
    AdjustedFullReward,
    AdjustedGuaranteedMinerReward,
    AdjustedDaoAddressReward,
    AdjustedXtnBuybackAddressReward,
    Fraction(5, 6),
    Fraction(1, 6)
  )

  def getBlockRewardShares(
      height: Height,
      fullBlockReward: Long,
      daoAddress: Option[Address],
      xtnBuybackAddress: Option[Address],
      blockchain: Blockchain
  ): BlockRewardShares = {
    val blockRewardDistributionHeight = blockchain.featureActivationHeight(BlockchainFeatures.BlockRewardDistribution).getOrElse(Height(Int.MaxValue))
    val cappedRewardHeight            = blockchain.featureActivationHeight(BlockchainFeatures.CappedReward).getOrElse(Height(Int.MaxValue))
    val ceaseXtnBuybackHeight         = blockchain.featureActivationHeight(BlockchainFeatures.CeaseXtnBuyback).getOrElse(Height(Int.MaxValue))
    val adjustedRewardDistributionHeight =
      blockchain.featureActivationHeight(BlockchainFeatures.AdjustedBlockRewardDistribution).getOrElse(Height(Int.MaxValue))

    if (height >= blockRewardDistributionHeight) {
      val modifiedXtnBuybackAddress = xtnBuybackAddress.filter { _ =>
        height < ceaseXtnBuybackHeight ||
        height < blockRewardDistributionHeight + blockchain.settings.functionalitySettings.xtnBuybackRewardPeriod
      }
      if (height >= cappedRewardHeight) {
        val distribution = if (height >= adjustedRewardDistributionHeight) AdjustedDistribution else DefaultDistribution

        if (fullBlockReward < distribution.guaranteedMinerReward) {
          BlockRewardShares(fullBlockReward, 0, 0)
        } else if (fullBlockReward < distribution.fullReward) {
          val remainder = fullBlockReward - distribution.guaranteedMinerReward
          calculateRewards(
            fullBlockReward,
            daoAddress.fold(0L)(_ => distribution.daoAddressRemainderPart(remainder)),
            modifiedXtnBuybackAddress.fold(0L)(_ => distribution.xtnBuybackAddressRemainderPart(remainder))
          )
        } else {
          calculateRewards(
            fullBlockReward,
            daoAddress.fold(0L)(_ => distribution.maxDaoAddressReward),
            modifiedXtnBuybackAddress.fold(0L)(_ => distribution.maxXtnBuybackAddressReward)
          )
        }
      } else {
        calculateRewards(fullBlockReward, CurrentBlockRewardPart.apply(fullBlockReward), daoAddress, modifiedXtnBuybackAddress)
      }
    } else BlockRewardShares(fullBlockReward, 0, 0)
  }.multiply(blockchain.blockRewardBoost(height))

  def getSortedBlockRewardShares(height: Int, fullBlockReward: Long, generator: Address, blockchain: Blockchain): Seq[(Address, Long)] = {
    val daoAddress        = blockchain.settings.functionalitySettings.daoAddressParsed.toOption.flatten
    val xtnBuybackAddress = blockchain.settings.functionalitySettings.xtnBuybackAddressParsed.toOption.flatten

    val rewardShares = getBlockRewardShares(Height(height), fullBlockReward, daoAddress, xtnBuybackAddress, blockchain)

    import com.wavesplatform.utils.byteStrOrdering

    (Seq(generator -> rewardShares.miner) ++
      daoAddress.map(_ -> rewardShares.daoAddress) ++
      xtnBuybackAddress.map(_ -> rewardShares.xtnBuybackAddress))
      .filter(_._2 > 0)
      .sortBy { case (addr, _) => ByteStr(addr.bytes) }
  }

  def getSortedBlockRewardShares(height: Int, generator: Address, blockchain: Blockchain): Seq[(Address, Long)] = {
    val fullBlockReward = blockchain.blockReward(height).getOrElse(0L)
    getSortedBlockRewardShares(height, fullBlockReward, generator, blockchain)
  }

  private def calculateRewards(
      blockReward: Long,
      addressReward: Long,
      daoAddress: Option[Address],
      xtnBuybackAddress: Option[Address]
  ): BlockRewardShares =
    calculateRewards(blockReward, daoAddress.fold(0L)(_ => addressReward), xtnBuybackAddress.fold(0L)(_ => addressReward))

  private def calculateRewards(blockReward: Long, daoAddressReward: Long, xtnBuybackReward: Long) =
    BlockRewardShares(
      blockReward - daoAddressReward - xtnBuybackReward,
      daoAddressReward,
      xtnBuybackReward
    )
}

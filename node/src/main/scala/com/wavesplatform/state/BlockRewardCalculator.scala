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

  /** Fixed shares used after the activation of [[BlockchainFeatures.AdjustedBlockRewardDistribution]]. They replace the default 2/2/2 distribution
    * (or its boosted 20/20/20 form) and are not affected by the reward boost.
    */
  val AdjustedFullReward: Long              = 20 * Constants.UnitsInWave
  val AdjustedDaoAddressReward: Long        = 10 * Constants.UnitsInWave
  val AdjustedXtnBuybackAddressReward: Long = 2 * Constants.UnitsInWave

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
        if (fullBlockReward < GuaranteedMinerReward) {
          BlockRewardShares(fullBlockReward, 0, 0)
        } else if (fullBlockReward < FullRewardInit) {
          calculateRewards(
            fullBlockReward,
            RemaindRewardAddressPart.apply(fullBlockReward - GuaranteedMinerReward),
            daoAddress,
            modifiedXtnBuybackAddress
          )
        } else if (height >= adjustedRewardDistributionHeight) {
          calculateRewards(
            AdjustedFullReward,
            daoAddress.fold(0L)(_ => AdjustedDaoAddressReward),
            modifiedXtnBuybackAddress.fold(0L)(_ => AdjustedXtnBuybackAddressReward)
          )
        } else {
          calculateRewards(fullBlockReward, MaxAddressReward, daoAddress, modifiedXtnBuybackAddress)
        }
      } else {
        calculateRewards(fullBlockReward, CurrentBlockRewardPart.apply(fullBlockReward), daoAddress, modifiedXtnBuybackAddress)
      }
    } else BlockRewardShares(fullBlockReward, 0, 0)
  }.multiply(blockchain.blockRewardBoost(height))

  /** Total amount of WAVES issued by the block at the given height. Equals to the boosted block reward before the activation of
    * [[BlockchainFeatures.AdjustedBlockRewardDistribution]] and to the sum of the fixed shares after it.
    */
  def getTotalBlockReward(height: Height, fullBlockReward: Long, blockchain: Blockchain): Long = {
    val daoAddress        = blockchain.settings.functionalitySettings.daoAddressParsed.toOption.flatten
    val xtnBuybackAddress = blockchain.settings.functionalitySettings.xtnBuybackAddressParsed.toOption.flatten

    val shares = getBlockRewardShares(height, fullBlockReward, daoAddress, xtnBuybackAddress, blockchain)
    shares.miner + shares.daoAddress + shares.xtnBuybackAddress
  }

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

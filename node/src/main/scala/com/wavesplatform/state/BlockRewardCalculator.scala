package com.wavesplatform.state

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.settings.Constants
import com.wavesplatform.state.diffs.BlockDiffer.Fraction

object BlockRewardCalculator {

  case class BlockRewardShares(miner: Long, daoAddress: Long, xtnBuybackAddress: Long)

  val CurrentBlockRewardPart: Fraction   = Fraction(1, 3)
  val RemaindRewardAddressPart: Fraction = Fraction(1, 2)

  val FullRewardInit: Long        = 6 * Constants.UnitsInWave
  val MaxAddressReward: Long      = 2 * Constants.UnitsInWave
  val GuaranteedMinerReward: Long = 2 * Constants.UnitsInWave

  def getBlockRewardShares(
      height: Int,
      fullBlockReward: Long,
      daoAddress: Option[Address],
      xtnBuybackAddress: Option[Address],
      blockchain: Blockchain
  ): BlockRewardShares = {
    val blockRewardDistributionHeight = blockchain.featureActivationHeight(BlockchainFeatures.BlockRewardDistribution.id).getOrElse(Int.MaxValue)
    val cappedRewardHeight            = blockchain.featureActivationHeight(BlockchainFeatures.CappedReward.id).getOrElse(Int.MaxValue)
    val ceaseXtnBuybackHeight         = blockchain.featureActivationHeight(BlockchainFeatures.CeaseXtnBuyback.id).getOrElse(Int.MaxValue)

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
        } else {
          calculateRewards(fullBlockReward, MaxAddressReward, daoAddress, modifiedXtnBuybackAddress)
        }
      } else {
        calculateRewards(fullBlockReward, CurrentBlockRewardPart.apply(fullBlockReward), daoAddress, modifiedXtnBuybackAddress)
      }
    } else BlockRewardShares(fullBlockReward, 0, 0)
  }

  def getSortedBlockRewardShares(height: Int, fullBlockReward: Long, generator: Address, blockchain: Blockchain): Seq[(Address, Long)] = {
    val daoAddress        = blockchain.settings.functionalitySettings.daoAddressParsed.toOption.flatten
    val xtnBuybackAddress = blockchain.settings.functionalitySettings.xtnBuybackAddressParsed.toOption.flatten

    val rewardShares = getBlockRewardShares(height, fullBlockReward, daoAddress, xtnBuybackAddress, blockchain)

    import com.wavesplatform.utils.byteStrOrdering

    (Seq(generator     -> rewardShares.miner) ++
      daoAddress.map(_ -> rewardShares.daoAddress) ++
      xtnBuybackAddress.map(_ -> rewardShares.xtnBuybackAddress))
      .filter(_._2 > 0)
      .sortBy { case (addr, _) => ByteStr(addr.bytes) }
  }

  def getSortedBlockRewardShares(height: Int, generator: Address, blockchain: Blockchain): Seq[(Address, Long)] = {
    val fullBlockReward = blockchain.blockReward(height).getOrElse(0L)
    getSortedBlockRewardShares(height, fullBlockReward, generator, blockchain)
  }

  private def calculateRewards(blockReward: Long, addressReward: Long, daoAddress: Option[Address], xtnBuybackAddress: Option[Address]) = {
    val daoAddressReward = daoAddress.fold(0L) { _ => addressReward }
    val xtnBuybackReward = xtnBuybackAddress.fold(0L) { _ => addressReward }
    BlockRewardShares(
      blockReward - daoAddressReward - xtnBuybackReward,
      daoAddressReward,
      xtnBuybackReward
    )
  }
}

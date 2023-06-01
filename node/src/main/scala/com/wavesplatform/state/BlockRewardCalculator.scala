package com.wavesplatform.state

import com.wavesplatform.account.Address
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.settings.Constants
import com.wavesplatform.state.diffs.BlockDiffer.Fraction

object BlockRewardCalculator {

  case class BlockReward(miner: Long, daoAddress: Long, xtnBuybackAddress: Long)

  val CurrentBlockRewardPart: Fraction   = Fraction(1, 3)
  val RemaindRewardAddressPart: Fraction = Fraction(1, 2)

  val FullRewardInit: Long        = 6 * Constants.UnitsInWave
  val MaxAddressReward: Long      = 2 * Constants.UnitsInWave
  val GuaranteedMinerReward: Long = 2 * Constants.UnitsInWave

  def getBlockReward(height: Int, daoAddress: Option[Address], xtnBuybackAddress: Option[Address], blockchain: Blockchain): BlockReward = {
    val blockReward                   = blockchain.lastBlockReward.getOrElse(0L)
    val blockRewardDistributionHeight = blockchain.featureActivationHeight(BlockchainFeatures.BlockRewardDistribution.id).getOrElse(Int.MaxValue)
    val cappedRewardHeight            = blockchain.featureActivationHeight(BlockchainFeatures.CappedReward.id).getOrElse(Int.MaxValue)
    val ceaseXtnBuybackHeight         = blockchain.featureActivationHeight(BlockchainFeatures.CeaseXtnBuyback.id).getOrElse(Int.MaxValue)

    if (height >= blockRewardDistributionHeight) {
      val modifiedXtnBuybackAddress = xtnBuybackAddress.filter { _ =>
        height < ceaseXtnBuybackHeight ||
        height < blockRewardDistributionHeight + blockchain.settings.functionalitySettings.xtnBuybackRewardPeriod
      }
      if (height >= cappedRewardHeight) {
        if (blockReward < GuaranteedMinerReward) {
          BlockReward(blockReward, 0, 0)
        } else if (blockReward < FullRewardInit) {
          calculateRewards(
            blockReward,
            RemaindRewardAddressPart.apply(blockReward - GuaranteedMinerReward),
            daoAddress,
            modifiedXtnBuybackAddress
          )
        } else {
          calculateRewards(blockReward, MaxAddressReward, daoAddress, modifiedXtnBuybackAddress)
        }
      } else {
        calculateRewards(blockReward, CurrentBlockRewardPart.apply(blockReward), daoAddress, modifiedXtnBuybackAddress)
      }
    } else BlockReward(blockReward, 0, 0)
  }

  private def calculateRewards(blockReward: Long, addressReward: Long, daoAddress: Option[Address], xtnBuybackAddress: Option[Address]) = {
    val daoAddressReward = daoAddress.fold(0L) { _ => addressReward }
    val xtnBuybackReward = xtnBuybackAddress.fold(0L) { _ => addressReward }
    BlockReward(
      blockReward - daoAddressReward - xtnBuybackReward,
      daoAddressReward,
      xtnBuybackReward
    )
  }
}

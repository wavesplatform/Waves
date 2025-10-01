package com.wavesplatform.consensus

import cats.syntax.either.*
import com.wavesplatform.account.Address
import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.{Blockchain, Height}

object GeneratingBalanceProvider {
  val MinimalEffectiveBalanceForGenerator1: Long = 1000000000000L
  val MinimalEffectiveBalanceForGenerator2: Long = 100000000000L

  private val FirstDepth  = 50
  private val SecondDepth = 1000

  def checkMiningAllowed(blockchain: Blockchain, height: Int, miner: Address, effectiveBalance: Long): Either[String, Unit] = {
    val smallerBalance = blockchain.isFeatureActivated(BlockchainFeatures.SmallerMinimalGeneratingBalance, height)
    for {
      _ <- Either.raiseUnless(
        !smallerBalance && effectiveBalance >= MinimalEffectiveBalanceForGenerator1 || smallerBalance && effectiveBalance >= MinimalEffectiveBalanceForGenerator2
      ) {
        s"Balance $effectiveBalance of $miner is lower than required for generation"
      }
      period = blockchain.generationPeriodOf(Height(height))
      _ <- Either.raiseUnless(blockchain.isCommitted(miner, height))(s"$miner is not committed on the generation period started at ${period.start}")
    } yield ()
  }

  // noinspection ScalaStyle
  def isEffectiveBalanceValid(blockchain: Blockchain, height: Int, block: Block, effectiveBalance: Long): Boolean =
    block.header.timestamp < blockchain.settings.functionalitySettings.minimalGeneratingBalanceAfter || (block.header.timestamp >= blockchain.settings.functionalitySettings.minimalGeneratingBalanceAfter && effectiveBalance >= MinimalEffectiveBalanceForGenerator1) ||
      blockchain.activatedFeatures
        .get(BlockchainFeatures.SmallerMinimalGeneratingBalance.id)
        .exists(height >= _) && effectiveBalance >= MinimalEffectiveBalanceForGenerator2

  def unchallengedBalance(blockchain: Blockchain, account: Address, blockId: Option[BlockId] = None): Long = {
    val height = blockId.flatMap(blockchain.heightOf).getOrElse(blockchain.height)
    val depth  = if (height >= blockchain.settings.functionalitySettings.generationBalanceDepthFrom50To1000AfterHeight) SecondDepth else FirstDepth

    blockchain.unbannedEffectiveBalance(account, depth, blockId)
  }

  def balance(blockchain: Blockchain, account: Address, blockId: Option[BlockId] = None): Long = {
    val height = blockId.flatMap(blockchain.heightOf).getOrElse(blockchain.height)
    val depth  = if (height >= blockchain.settings.functionalitySettings.generationBalanceDepthFrom50To1000AfterHeight) SecondDepth else FirstDepth

    val maybeChallengedMiner = blockchain.blockHeader(height + 1).flatMap(_.header.challengedHeader).map(_.generator.toAddress)
    blockchain.effectiveBalance(account, depth, blockId) + maybeChallengedMiner.map(blockchain.effectiveBalance(_, depth, blockId)).getOrElse(0L)
  }
}

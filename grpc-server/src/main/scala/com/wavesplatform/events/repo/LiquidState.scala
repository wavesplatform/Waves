package com.wavesplatform.events.repo

import cats.syntax.monoid._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.{BlockAppended, BlockchainUpdated, MicroBlockAppended}

case class LiquidState(
    keyBlock: BlockAppended,
    microBlocks: Seq[MicroBlockAppended]
) {
  def solidify(): BlockAppended = LiquidState.solidify(keyBlock, microBlocks)

  def toSeq: Seq[BlockchainUpdated] = Seq(keyBlock) ++ microBlocks
  def totalBlockId: ByteStr         = microBlocks.lastOption.fold(keyBlock.id)(_.id)
}

object LiquidState {
  def solidify(keyBlock: BlockAppended, microBlocks: Seq[MicroBlockAppended]): BlockAppended = {
    val toId             = microBlocks.lastOption.fold(keyBlock.id)(_.id)
    val signature        = microBlocks.lastOption.fold(keyBlock.block.signature)(_.microBlock.totalResBlockSig)
    val transactionsRoot = microBlocks.lastOption.fold(keyBlock.block.header.transactionsRoot)(_.totalTransactionsRoot)

    val transactionData         = microBlocks.foldLeft(keyBlock.block.transactionData)((txs, mb) => txs ++ mb.microBlock.transactionData)
    val blockStateUpdate        = microBlocks.foldLeft(keyBlock.blockStateUpdate)((upd, mb) => upd.combine(mb.microBlockStateUpdate))
    val transactionStateUpdates = microBlocks.foldLeft(keyBlock.transactionStateUpdates)((upds, mb) => upds ++ mb.transactionStateUpdates)
    val transactionMetadata     = microBlocks.foldLeft(keyBlock.transactionMetadata)((upds, mb) => upds ++ mb.transactionMetadata)
    val referencedAssets        = microBlocks.foldLeft(keyBlock.referencedAssets)((upds, mb) => upds ++ mb.referencedAssets)

    BlockAppended(
      id = toId,
      height = keyBlock.height,
      block = keyBlock.block.copy(
        header = keyBlock.block.header.copy(transactionsRoot = transactionsRoot),
        signature = signature,
        transactionData = transactionData
      ),
      updatedWavesAmount = keyBlock.updatedWavesAmount,
      vrf = keyBlock.vrf,
      activatedFeatures = keyBlock.activatedFeatures,
      rewardShares = keyBlock.rewardShares,
      blockStateUpdate = blockStateUpdate,
      transactionStateUpdates = transactionStateUpdates,
      transactionMetadata = transactionMetadata,
      referencedAssets = referencedAssets
    )
  }
}

package com.wavesplatform.events.repo

import cats.syntax.monoid._
import com.wavesplatform.events.{BlockAppended, BlockchainUpdated, MicroBlockAppended}

case class LiquidState(
    keyBlock: BlockAppended,
    microBlocks: Seq[MicroBlockAppended]
) {
  def solidify(): BlockAppended = {
    val toId             = microBlocks.lastOption.fold(keyBlock.toId)(_.toId)
    val signature        = microBlocks.lastOption.fold(keyBlock.block.signature)(_.microBlock.totalResBlockSig)
    val transactionsRoot = microBlocks.lastOption.fold(keyBlock.block.header.transactionsRoot)(_.totalTransactionsRoot)

    val transactionData         = microBlocks.foldLeft(keyBlock.block.transactionData)((txs, mb) => txs ++ mb.microBlock.transactionData)
    val blockStateUpdate        = microBlocks.foldLeft(keyBlock.blockStateUpdate)((upd, mb) => upd.combine(mb.microBlockStateUpdate))
    val transactionStateUpdates = microBlocks.foldLeft(keyBlock.transactionStateUpdates)((upds, mb) => upds ++ mb.transactionStateUpdates)

    BlockAppended(
      toId = toId,
      toHeight = keyBlock.toHeight,
      block = keyBlock.block.copy(
        header = keyBlock.block.header.copy(transactionsRoot = transactionsRoot),
        signature = signature,
        transactionData = transactionData
      ),
      updatedWavesAmount = keyBlock.updatedWavesAmount,
      blockStateUpdate = blockStateUpdate,
      transactionStateUpdates = transactionStateUpdates
    )
  }

  def toSeq: Seq[BlockchainUpdated] = Seq(keyBlock) ++ microBlocks
}

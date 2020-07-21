package com.wavesplatform.events.repo

import cats.syntax.monoid._
import com.wavesplatform.events.{BlockAppended, MicroBlockAppended}

case class LiquidState(
    keyBlock: BlockAppended,
    microBlocks: Seq[MicroBlockAppended]
) {
  def solidify(): BlockAppended = {
    val totalResBlockSig        = microBlocks.lastOption.fold(keyBlock.block.signature)(_.microBlock.totalResBlockSig)
    val transactionData         = microBlocks.foldLeft(keyBlock.block.transactionData)((txs, mb) => txs ++ mb.microBlock.transactionData)
    val blockStateUpdate        = microBlocks.foldLeft(keyBlock.blockStateUpdate)((upd, mb) => upd.combine(mb.microBlockStateUpdate))
    val transactionStateUpdates = microBlocks.foldLeft(keyBlock.transactionStateUpdates)((upds, mb) => upds ++ mb.transactionStateUpdates)

    // todo make sure generationSignature and transactionsRoot are correct in Block
    // not touching them for now
    BlockAppended(
      toId = totalResBlockSig,
      toHeight = keyBlock.toHeight,
      block = keyBlock.block.copy(
        signature = totalResBlockSig,
        transactionData = transactionData
      ),
      updatedWavesAmount = keyBlock.updatedWavesAmount,
      blockStateUpdate = blockStateUpdate,
      transactionStateUpdates = transactionStateUpdates
    )
  }
}

package com.wavesplatform.state2

import scorex.block.Block.BlockId
import scorex.block.{Block, MicroBlock}
import scorex.transaction.{AssetId, DiscardedMicroBlocks, Transaction}


case class NgState private(base: Block, diffs: Map[BlockId, BlockDiff], micros: List[MicroBlock]) {

  lazy val lastMicroTotalSig: Option[ByteStr] =
    micros.headOption.map(_.totalResBlockSig)

  lazy val bestLiquidBlockId: AssetId =
    lastMicroTotalSig.getOrElse(base.uniqueId)

  lazy val transactions: Seq[Transaction] = base.transactionData ++ micros.map(_.transactionData).reverse.flatten

  lazy val bestLiquidBlock: Block =
    if (micros.isEmpty) {
      base
    } else {
      base.copy(signerData = base.signerData.copy(signature = micros.head.totalResBlockSig),
        transactionData = transactions)
    }

  lazy val bestLiquidDiff: BlockDiff =
    diffs(bestLiquidBlockId)
      .copy(heightDiff = 1)

  def contains(blockId: BlockId): Boolean = diffs.contains(blockId)

  def microBlock(id: BlockId): Option[MicroBlock] = micros.find(_.totalResBlockSig == id)

  def forgeBlock(id: BlockId): Option[(Block, DiscardedMicroBlocks)] = {
    val ms = micros.reverse
    if (base.uniqueId == id) {
      Some((base, ms))
    } else if (!ms.exists(_.totalResBlockSig == id)) None
    else {
      val (accumulatedTxs, maybeFound) = ms.foldLeft((List.empty[Transaction], Option.empty[(ByteStr, DiscardedMicroBlocks)])) { case ((accumulated, maybeDiscarded), micro) =>
        maybeDiscarded match {
          case Some((sig, discarded)) => (accumulated, Some((sig, micro +: discarded)))
          case None =>
            if (micro.totalResBlockSig == id)
              (accumulated ++ micro.transactionData, Some((micro.totalResBlockSig, Seq.empty[MicroBlock])))
            else
              (accumulated ++ micro.transactionData, None)
        }
      }
      maybeFound.map { case (sig, discardedMicroblocks) => (
        base.copy(signerData = base.signerData.copy(signature = sig), transactionData = base.transactionData ++ accumulatedTxs),
        discardedMicroblocks)
      }
    }
  }
}

object NgState {

  def apply(base: Block, diff: BlockDiff): NgState =
    NgState(base, Map(base.uniqueId -> diff), List.empty)

  implicit class NgStateExt(n: NgState) {
    def +(m: MicroBlock, diff: BlockDiff): NgState = NgState(n.base, n.diffs + (m.totalResBlockSig -> diff), m +: n.micros)
  }

}


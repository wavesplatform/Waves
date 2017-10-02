package com.wavesplatform.state2

import scorex.block.Block.BlockId
import scorex.block.{Block, MicroBlock}
import scorex.transaction.History.BlockMinerInfo
import scorex.transaction.{AssetId, DiscardedMicroBlocks, Transaction}


case class NgState private(base: Block, diffs: Map[BlockId, (BlockDiff, Long)], micros: List[MicroBlock], acceptedFeatures: Set[Short]) {

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
    diffs(bestLiquidBlockId)._1
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

  def bestLastBlockInfo(maxTimeStamp: Long): BlockMinerInfo = {
    val blockId = micros.find(micro => diffs(micro.totalResBlockSig)._2 <= maxTimeStamp)
      .map(_.totalResBlockSig)
      .getOrElse(base.uniqueId)
    BlockMinerInfo(base.consensusData, base.timestamp, blockId)
  }
}

object NgState {

  def apply(base: Block, diff: BlockDiff, timestamp: Long, acceptedFeatures: Set[Short]): NgState =
    NgState(base, Map(base.uniqueId -> ((diff, timestamp))), List.empty, acceptedFeatures)

  implicit class NgStateExt(n: NgState) {
    def +(m: MicroBlock, diff: BlockDiff, timestamp: Long): NgState = NgState(n.base, n.diffs + (m.totalResBlockSig -> ((diff, timestamp))), m +: n.micros, n.acceptedFeatures)
  }


}


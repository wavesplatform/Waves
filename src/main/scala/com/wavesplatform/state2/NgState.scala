package com.wavesplatform.state2


import java.util.concurrent.TimeUnit

import cats.kernel.Monoid
import com.google.common.cache.CacheBuilder
import scorex.block.Block.BlockId
import scorex.block.{Block, MicroBlock}
import scorex.transaction.History.BlockMinerInfo
import scorex.transaction.{AssetId, DiscardedMicroBlocks, Transaction}
import scala.collection.mutable.{Map => MMap, ListBuffer => MList}


class NgState(val base: Block, val baseBlockDiff: BlockDiff, val acceptedFeatures: Set[Short]) {

  private val microDiffs: MMap[BlockId, (BlockDiff, Long)] = MMap.empty
  private val micros: MList[MicroBlock] = MList.empty
  private val totalBlockDiffCache = CacheBuilder.newBuilder()
    .maximumSize(2L)
    .expireAfterWrite(10, TimeUnit.MINUTES)
    .build[BlockId, BlockDiff]()

  def microBlockIds: Seq[BlockId] = micros.map(_.totalResBlockSig).toList

  private def diffFor(totalResBlockSig: BlockId): BlockDiff =
    if (totalResBlockSig == base.uniqueId)
      baseBlockDiff
    else
      totalBlockDiffCache.get(totalResBlockSig, () => {
        val prevResBlockSig = micros.find(_.totalResBlockSig == totalResBlockSig).get.prevResBlockSig
        val prevResBlockDiff = totalBlockDiffCache.get(prevResBlockSig, () => diffFor(prevResBlockSig))
        val currentMicroDiff = microDiffs(totalResBlockSig)._1
        Monoid.combine(prevResBlockDiff, currentMicroDiff)
      })

  def bestLiquidBlockId: AssetId =
    micros.headOption.map(_.totalResBlockSig).getOrElse(base.uniqueId)

  def lastMicroBlock : Option[MicroBlock] = micros.headOption

  def transactions: Seq[Transaction] = base.transactionData ++ micros.map(_.transactionData).reverse.flatten

  def bestLiquidBlock: Block =
    if (micros.isEmpty) {
      base
    } else {
      base.copy(signerData = base.signerData.copy(signature = micros.head.totalResBlockSig),
        transactionData = transactions)
    }

  def totalDiffOf(id: BlockId): Option[(Block, BlockDiff, DiscardedMicroBlocks)] =
    forgeBlock(id).map { case (b, txs) => (b, diffFor(id), txs) }

  def bestLiquidDiff: BlockDiff = micros.headOption.map(m => totalDiffOf(m.totalResBlockSig).get._2).getOrElse(baseBlockDiff).copy(heightDiff = 1)

  def contains(blockId: BlockId): Boolean = base.uniqueId == blockId || microDiffs.contains(blockId)

  def microBlock(id: BlockId): Option[MicroBlock] = micros.find(_.totalResBlockSig == id)

  private def forgeBlock(id: BlockId): Option[(Block, DiscardedMicroBlocks)] = {
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
    val blockId = micros.find(micro => microDiffs(micro.totalResBlockSig)._2 <= maxTimeStamp)
      .map(_.totalResBlockSig)
      .getOrElse(base.uniqueId)
    BlockMinerInfo(base.consensusData, base.timestamp, blockId)
  }

  def append(m: MicroBlock, diff: BlockDiff, timestamp: Long): Unit = {
    microDiffs.put(m.totalResBlockSig, (diff, timestamp))
    micros.prepend(m)
  }
}
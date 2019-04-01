package com.wavesplatform.state

import java.util.concurrent.TimeUnit

import cats.kernel.Monoid
import com.google.common.cache.CacheBuilder
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.{DiscardedMicroBlocks, Transaction}
import com.wavesplatform.utils.ScorexLogging

import scala.collection.mutable.{ListBuffer => MList, Map => MMap}

/* This is not thread safe, used only from BlockchainUpdaterImpl */
class NgState(val base: Block, val baseBlockDiff: Diff, val baseBlockCarry: Long, val approvedFeatures: Set[Short]) extends ScorexLogging {
  private[this] val MaxTotalDiffs = 15

  private val microDiffs: MMap[BlockId, (Diff, Long, Long)] = MMap.empty  // microDiff, carryFee, timestamp
  private val microBlocks: MList[MicroBlock]                = MList.empty // fresh head

  def microBlockIds: Seq[BlockId] =
    microBlocks.map(_.totalResBlockSig)

  def diffFor(totalResBlockSig: BlockId): (Diff, Long) =
    if (totalResBlockSig == base.uniqueId)
      (baseBlockDiff, baseBlockCarry)
    else
      internalCaches.blockDiffCache.get(
        totalResBlockSig, { () =>
          microBlocks.find(_.totalResBlockSig == totalResBlockSig) match {
            case Some(current) =>
              val (prevDiff, prevCarry)    = this.diffFor(current.prevResBlockSig)
              val (currDiff, currCarry, _) = this.microDiffs(totalResBlockSig)
              (Monoid.combine(prevDiff, currDiff), prevCarry + currCarry)

            case None =>
              (Diff.empty, 0L)
          }
        }
      )

  def bestLiquidBlockId: BlockId =
    microBlocks.headOption.map(_.totalResBlockSig).getOrElse(base.uniqueId)

  def lastMicroBlock: Option[MicroBlock] =
    microBlocks.headOption

  def transactions: Seq[Transaction] =
    base.transactionData.toVector ++ microBlocks.view.map(_.transactionData).reverse.flatten

  def bestLiquidBlock: Block =
    if (microBlocks.isEmpty)
      base
    else
      internalCaches.bestBlockCache match {
        case Some(cachedBlock) =>
          cachedBlock

        case None =>
          val block = base.copy(signerData = base.signerData.copy(signature = microBlocks.head.totalResBlockSig), transactionData = transactions)
          internalCaches.bestBlockCache = Some(block)
          block
      }

  def totalDiffOf(id: BlockId): Option[(Block, Diff, Long, DiscardedMicroBlocks)] =
    forgeBlock(id).map {
      case (block, discarded) =>
        val (diff, carry) = this.diffFor(id)
        (block, diff, carry, discarded)
    }

  def bestLiquidDiff: Diff =
    microBlocks.headOption.fold(baseBlockDiff)(m => diffFor(m.totalResBlockSig)._1)

  def contains(blockId: BlockId): Boolean = base.uniqueId == blockId || microDiffs.contains(blockId)

  def microBlock(id: BlockId): Option[MicroBlock] = microBlocks.find(_.totalResBlockSig == id)

  def bestLastBlockInfo(maxTimeStamp: Long): BlockMinerInfo = {
    val blockId = microBlocks
      .find(micro => microDiffs(micro.totalResBlockSig)._3 <= maxTimeStamp)
      .map(_.totalResBlockSig)
      .getOrElse(base.uniqueId)
    BlockMinerInfo(base.consensusData, base.timestamp, blockId)
  }

  def append(m: MicroBlock, diff: Diff, microblockCarry: Long, timestamp: Long): Unit = {
    microDiffs.put(m.totalResBlockSig, (diff, microblockCarry, timestamp))
    microBlocks.prepend(m)
    internalCaches.invalidate(m.totalResBlockSig)
  }

  def carryFee: Long = baseBlockCarry + microDiffs.values.map(_._2).sum

  private[this] def forgeBlock(blockId: BlockId): Option[(Block, DiscardedMicroBlocks)] =
    internalCaches.forgedBlockCache.get(
      blockId, { () =>
        val microBlocksAsc = microBlocks.reverse

        if (base.uniqueId == blockId) {
          Some((base, microBlocksAsc))
        } else if (!microBlocksAsc.exists(_.totalResBlockSig == blockId)) None
        else {
          val (accumulatedTxs, maybeFound) = microBlocksAsc.foldLeft((Vector.empty[Transaction], Option.empty[(ByteStr, DiscardedMicroBlocks)])) {
            case ((accumulated, Some((sig, discarded))), micro) =>
              (accumulated, Some((sig, micro +: discarded)))

            case ((accumulated, None), micro) =>
              val found = Some((micro.totalResBlockSig, Seq.empty[MicroBlock])).filter(_._1 == blockId)
              (accumulated ++ micro.transactionData, found)
          }

          maybeFound.map {
            case (sig, discarded) =>
              (base.copy(signerData = base.signerData.copy(signature = sig), transactionData = base.transactionData ++ accumulatedTxs), discarded)
          }
        }
      }
    )

  private[this] object internalCaches {
    val blockDiffCache = CacheBuilder
      .newBuilder()
      .maximumSize(MaxTotalDiffs)
      .expireAfterWrite(10, TimeUnit.MINUTES)
      .build[BlockId, (Diff, Long)]()

    val forgedBlockCache = CacheBuilder
      .newBuilder()
      .maximumSize(MaxTotalDiffs)
      .expireAfterWrite(10, TimeUnit.MINUTES)
      .build[BlockId, Option[(Block, DiscardedMicroBlocks)]]()

    @volatile
    var bestBlockCache = Option.empty[Block]

    def invalidate(newBlockId: BlockId): Unit = {
      forgedBlockCache.invalidateAll()
      blockDiffCache.invalidate(newBlockId)
      bestBlockCache = None
    }
  }
}

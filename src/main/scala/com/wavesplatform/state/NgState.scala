package com.wavesplatform.state

import java.util.concurrent.TimeUnit

import cats.kernel.Monoid
import com.google.common.cache.CacheBuilder
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.{DiscardedMicroBlocks, Transaction}
import com.wavesplatform.utils.ScorexLogging

import scala.collection.mutable.{ArrayBuffer => MList, Map => MMap}

/* This is not thread safe, used only from BlockchainUpdaterImpl */
class NgState(val base: Block, val baseBlockDiff: Diff, val baseBlockCarry: Long, val approvedFeatures: Set[Short]) extends ScorexLogging {
  private[this] val MaxTotalDiffs = 100

  private[this] val microDiffs: MMap[BlockId, (Diff, Long, Long)] = MMap.empty  // microDiff, carryFee, timestamp
  private[this] val micros: MList[MicroBlock]                     = MList.empty // fresh head

  private[this] val blockDiffCache = CacheBuilder
    .newBuilder()
    .maximumSize(MaxTotalDiffs)
    .expireAfterWrite(10, TimeUnit.MINUTES)
    .build[BlockId, (Diff, Long)]()

  private[this] val forgedBlockCache = CacheBuilder
    .newBuilder()
    .maximumSize(MaxTotalDiffs)
    .expireAfterWrite(10, TimeUnit.MINUTES)
    .build[BlockId, Option[(Block, DiscardedMicroBlocks)]]()

  def microBlockIds: Seq[BlockId] =
    micros.map(_.totalResBlockSig)

  def diffFor(totalResBlockSig: BlockId): (Diff, Long) =
    if (totalResBlockSig == base.uniqueId)
      (baseBlockDiff, baseBlockCarry)
    else
      blockDiffCache.get(
        totalResBlockSig, { () =>
          micros.find(_.totalResBlockSig == totalResBlockSig) match {
            case Some(current) =>
              val prevResBlockSig          = current.prevResBlockSig
              val (prevDiff, prevCarry)    = diffFor(prevResBlockSig)
              val (currDiff, currCarry, _) = microDiffs(totalResBlockSig)
              (Monoid.combine(prevDiff, currDiff), prevCarry + currCarry)

            case None =>
              (Diff.empty, 0L)
          }
        }
      )

  def bestLiquidBlockId: BlockId =
    micros.headOption.map(_.totalResBlockSig).getOrElse(base.uniqueId)

  def lastMicroBlock: Option[MicroBlock] =
    micros.headOption

  def transactions: Seq[Transaction] =
    base.transactionData ++ micros.map(_.transactionData).reverse.flatten

  def bestLiquidBlock: Block =
    if (micros.isEmpty) base
    else base.copy(signerData = base.signerData.copy(signature = micros.head.totalResBlockSig), transactionData = transactions)

  def totalDiffOf(id: BlockId): Option[(Block, Diff, Long, DiscardedMicroBlocks)] =
    forgeBlock(id).map {
      case (block, discarded) =>
        val (diff, carry) = this.diffFor(id)
        (block, diff, carry, discarded)
    }

  def bestLiquidDiff: Diff =
    micros.headOption.fold(baseBlockDiff)(m => diffFor(m.totalResBlockSig)._1)

  def contains(blockId: BlockId): Boolean = base.uniqueId == blockId || microDiffs.contains(blockId)

  def microBlock(id: BlockId): Option[MicroBlock] = micros.find(_.totalResBlockSig == id)

  def bestLastBlockInfo(maxTimeStamp: Long): BlockMinerInfo = {
    val blockId = micros
      .find(micro => microDiffs(micro.totalResBlockSig)._3 <= maxTimeStamp)
      .map(_.totalResBlockSig)
      .getOrElse(base.uniqueId)
    BlockMinerInfo(base.consensusData, base.timestamp, blockId)
  }

  def append(m: MicroBlock, diff: Diff, microblockCarry: Long, timestamp: Long): Unit = {
    microDiffs.put(m.totalResBlockSig, (diff, microblockCarry, timestamp))
    micros.prepend(m)
    forgedBlockCache.invalidateAll()
    blockDiffCache.invalidate(m.totalResBlockSig)
  }

  def carryFee: Long = baseBlockCarry + microDiffs.values.map(_._2).sum

  private[this] def forgeBlock(blockId: BlockId): Option[(Block, DiscardedMicroBlocks)] =
    forgedBlockCache.get(
      blockId, { () =>
        val microsFromEnd = micros.reverse

        if (base.uniqueId == blockId) {
          Some((base, microsFromEnd))
        } else if (!microsFromEnd.exists(_.totalResBlockSig == blockId)) None
        else {
          val (accumulatedTxs, maybeFound) = microsFromEnd.foldLeft((MList.empty[Transaction], Option.empty[(ByteStr, DiscardedMicroBlocks)])) {
            case ((accumulated, Some((sig, discarded))), micro) =>
              (accumulated, Some((sig, micro +: discarded)))

            case ((accumulated, None), micro) =>
              val found = Some((micro.totalResBlockSig, Seq.empty[MicroBlock])).filter(_._1 == blockId)
              (accumulated ++= micro.transactionData, found)
          }

          maybeFound.map {
            case (sig, discarded) =>
              (base.copy(signerData = base.signerData.copy(signature = sig), transactionData = base.transactionData ++ accumulatedTxs), discarded)
          }
        }
      }
    )
}

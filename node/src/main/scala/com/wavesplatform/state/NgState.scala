package com.wavesplatform.state

import java.util.concurrent.TimeUnit

import cats.kernel.Monoid
import com.google.common.cache.CacheBuilder
import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.{DiscardedMicroBlocks, Transaction}

import scala.collection.mutable.{ListBuffer => MList, Map => MMap}

/* This is not thread safe, used only from BlockchainUpdaterImpl */
class NgState(val base: Block, val baseBlockDiff: Diff, val baseBlockCarry: Long, val approvedFeatures: Set[Short]) extends ScorexLogging {

  private val MaxTotalDiffs = 3

  private val microDiffs: MMap[BlockId, (Diff, Long, Long)] = MMap.empty  // microDiff, carryFee, timestamp
  private val micros: MList[MicroBlock]                     = MList.empty // fresh head

  private val totalBlockDiffCache = CacheBuilder
    .newBuilder()
    .maximumSize(MaxTotalDiffs)
    .expireAfterWrite(10, TimeUnit.MINUTES)
    .build[BlockId, (Diff, Long)]()

  def microBlockIds: Seq[BlockId] = micros.map(_.totalResBlockSig).toList

  def diffFor(totalResBlockSig: BlockId): (Diff, Long) =
    if (totalResBlockSig == base.uniqueId)
      (baseBlockDiff, baseBlockCarry)
    else
      Option(totalBlockDiffCache.getIfPresent(totalResBlockSig)) match {
        case Some(d) => d
        case None =>
          micros.find(_.totalResBlockSig == totalResBlockSig) match {
            case Some(current) =>
              val prevResBlockSig          = current.prevResBlockSig
              val (prevDiff, prevCarry)    = Option(totalBlockDiffCache.getIfPresent(prevResBlockSig)).getOrElse(diffFor(prevResBlockSig))
              val (currDiff, currCarry, _) = microDiffs(totalResBlockSig)
              val r                        = (Monoid.combine(prevDiff, currDiff), prevCarry + currCarry)
              totalBlockDiffCache.put(totalResBlockSig, r)
              r

            case None =>
              (Diff.empty, 0L)
          }
      }

  def bestLiquidBlockId: BlockId =
    micros.headOption.map(_.totalResBlockSig).getOrElse(base.uniqueId)

  def lastMicroBlock: Option[MicroBlock] = micros.headOption

  def transactions: Seq[Transaction] = base.transactionData ++ micros.map(_.transactionData).reverse.flatten

  def bestLiquidBlock: Block =
    if (micros.isEmpty) {
      base
    } else {
      base.copy(signerData = base.signerData.copy(signature = micros.head.totalResBlockSig), transactionData = transactions)
    }

  def totalDiffOf(id: BlockId): Option[(Block, Diff, Long, DiscardedMicroBlocks)] =
    forgeBlock(id).map {
      case (b, txs) =>
        val (d, c) = diffFor(id)
        (b, d, c, txs)
    }

  def bestLiquidDiff: Diff = micros.headOption.fold(baseBlockDiff)(m => diffFor(m.totalResBlockSig)._1)

  def contains(blockId: BlockId): Boolean = base.uniqueId == blockId || microDiffs.contains(blockId)

  def microBlock(id: BlockId): Option[MicroBlock] = micros.find(_.totalResBlockSig == id)

  private def forgeBlock(id: BlockId): Option[(Block, DiscardedMicroBlocks)] = {
    val ms = micros.reverse
    if (base.uniqueId == id) {
      Some((base, ms))
    } else if (!ms.exists(_.totalResBlockSig == id)) None
    else {
      val (accumulatedTxs, maybeFound) = ms.foldLeft((List.empty[Transaction], Option.empty[(ByteStr, DiscardedMicroBlocks)])) {
        case ((accumulated, maybeDiscarded), micro) =>
          maybeDiscarded match {
            case Some((sig, discarded)) => (accumulated, Some((sig, micro +: discarded)))
            case None =>
              if (micro.totalResBlockSig == id)
                (accumulated ++ micro.transactionData, Some((micro.totalResBlockSig, Seq.empty[MicroBlock])))
              else
                (accumulated ++ micro.transactionData, None)
          }
      }
      maybeFound.map {
        case (sig, discardedMicroblocks) =>
          (base.copy(signerData = base.signerData.copy(signature = sig), transactionData = base.transactionData ++ accumulatedTxs),
           discardedMicroblocks)
      }
    }
  }

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
  }

  def carryFee: Long = baseBlockCarry + microDiffs.values.map(_._2).sum
}

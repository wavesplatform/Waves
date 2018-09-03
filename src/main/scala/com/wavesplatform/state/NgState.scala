package com.wavesplatform.state

import java.util.concurrent.TimeUnit

import cats.kernel.Monoid
import com.google.common.cache.CacheBuilder
import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.transaction.{DiscardedMicroBlocks, Transaction}

import scala.collection.mutable.{ListBuffer => MList, Map => MMap}

class NgState(val base: Block, val baseBlockDiff: Diff, val baseBlockCarry: Long, val approvedFeatures: Set[Short]) extends ScorexLogging {

  private val MaxTotalDiffs = 3

  private val microDiffs: MMap[BlockId, (Diff, Long)] = MMap.empty
  private val micros: MList[MicroBlock]               = MList.empty // fresh head
  private val totalBlockDiffCache = CacheBuilder
    .newBuilder()
    .maximumSize(MaxTotalDiffs)
    .expireAfterWrite(10, TimeUnit.MINUTES)
    .build[BlockId, Diff]()
  var carry: Long = baseBlockCarry ///make this per id

  def microBlockIds: Seq[BlockId] = micros.map(_.totalResBlockSig).toList

  private def diffFor(totalResBlockSig: BlockId): (Diff, Long) =
    if (totalResBlockSig == base.uniqueId)
      (baseBlockDiff, baseBlockCarry)
    else
      Option(totalBlockDiffCache.getIfPresent(totalResBlockSig)) match {
        case Some(d) => (d, carry)
        case None =>
          val prevResBlockSig  = micros.find(_.totalResBlockSig == totalResBlockSig).get.prevResBlockSig
          val prevResBlockDiff = Option(totalBlockDiffCache.getIfPresent(prevResBlockSig)).getOrElse(diffFor(prevResBlockSig)._1)
          val currentMicroDiff = microDiffs(totalResBlockSig)._1
          val r                = Monoid.combine(prevResBlockDiff, currentMicroDiff)
//          Console.err.println(s"<==> NGS diffFor prev=$prevResBlockDiff") ///
//          Console.err.println(s"<==> NGS diffFor curr=$currentMicroDiff") ///
          totalBlockDiffCache.put(totalResBlockSig, r)
          (r, carry)
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
//        Console.err.println(s"<==> NGS totalDiff $d carry $c") ///
        (b, d, c, txs)
    }

  def bestLiquidDiff: Diff = micros.headOption.fold(baseBlockDiff)(m => totalDiffOf(m.totalResBlockSig).get._2)

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
      .find(micro => microDiffs(micro.totalResBlockSig)._2 <= maxTimeStamp)
      .map(_.totalResBlockSig)
      .getOrElse(base.uniqueId)
    BlockMinerInfo(base.consensusData, base.timestamp, blockId)
  }

  def append(m: MicroBlock, diff: Diff, microblockCarry: Long, timestamp: Long): Unit = {
    microDiffs.put(m.totalResBlockSig, (diff, timestamp))
    micros.prepend(m)
    carry += microblockCarry
    Console.err.println(s"<==> NGS append: diff $diff")                          ///
    Console.err.println(s"<==> NGS append: carry $microblockCarry total $carry") ///
  }

  def carryFee: Long = carry

  Console.err.println(s"<==> NGS mdiffs=$microDiffs baseCarry=$baseBlockCarry") ///
}

package com.wavesplatform.state

import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.{Lock, ReentrantReadWriteLock}

import cats.kernel.Monoid
import com.google.common.cache.CacheBuilder
import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.{DiscardedMicroBlocks, Transaction}

class NgState(val base: Block, val baseBlockDiff: Diff, val baseBlockCarry: Long, val approvedFeatures: Set[Short]) extends ScorexLogging {

  private val MaxTotalDiffs = 3

  private val state = new SynchronizedAppendState[MicroBlock, BlockId, (Diff, Long, Long)](_.totalResBlockSig)

  private def microDiffs = state.mapping // microDiff, carryFee, timestamp
  private def micros     = state.stack   // fresh head

  private val totalBlockDiffCache = CacheBuilder
    .newBuilder()
    .maximumSize(MaxTotalDiffs)
    .expireAfterWrite(10, TimeUnit.MINUTES)
    .build[BlockId, (Diff, Long)]()

  def microBlockIds: Seq[BlockId] = micros.map(_.totalResBlockSig).toList

  private def diffFor(totalResBlockSig: BlockId): (Diff, Long) =
    if (totalResBlockSig == base.uniqueId)
      (baseBlockDiff, baseBlockCarry)
    else
      Option(totalBlockDiffCache.getIfPresent(totalResBlockSig)) match {
        case Some(d) => d
        case None =>
          val prevResBlockSig          = micros.find(_.totalResBlockSig == totalResBlockSig).get.prevResBlockSig
          val (prevDiff, prevCarry)    = Option(totalBlockDiffCache.getIfPresent(prevResBlockSig)).getOrElse(diffFor(prevResBlockSig))
          val (currDiff, currCarry, _) = microDiffs(totalResBlockSig)
          val r                        = (Monoid.combine(prevDiff, currDiff), prevCarry + currCarry)
          totalBlockDiffCache.put(totalResBlockSig, r)
          r
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
    state.append(m, (diff, microblockCarry, timestamp))
  }

  def carryFee: Long = baseBlockCarry + microDiffs.values.map(_._2).sum
}

/**
  * Allow atomically appends to state
  * Return internal stack and mapping state without dirty reads
  */
private class SynchronizedAppendState[T, K, V](toKey: T => K) {
  private def inLock[R](l: Lock, f: => R) = {
    try {
      l.lock()
      val res = f
      res
    } finally {
      l.unlock()
    }
  }
  private val lock                     = new ReentrantReadWriteLock
  private def writeLock[B](f: => B): B = inLock(lock.writeLock(), f)
  private def readLock[B](f: => B): B  = inLock(lock.readLock(), f)

  @volatile private var internalStack = List.empty[T]
  @volatile private var internalMap   = Map.empty[K, V]

  /**
    * Stack state
    */
  def stack: List[T] = readLock(internalStack)

  /**
    * Mapping state
    */
  def mapping: Map[K, V] = readLock(internalMap)

  /**
    * Atomically appends to state both stack and map
    */
  def append(t: T, v: V): Unit = writeLock {
    internalStack = t :: internalStack
    internalMap = internalMap.updated(toKey(t), v)
  }
}

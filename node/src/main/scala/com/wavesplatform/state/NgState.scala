package com.wavesplatform.state

import java.util.concurrent.TimeUnit

import cats.kernel.Monoid
import com.google.common.cache.CacheBuilder
import com.wavesplatform.account.Address
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.transaction.{DiscardedMicroBlocks, Transaction}

import scala.collection.mutable.{ListBuffer => MList, Map => MMap}

/* This is not thread safe, used only from BlockchainUpdaterImpl */
class NgState(
    val base: Block,
    baseBlockDiff: Diff,
    baseBlockCarry: Long,
    baseBlockTotalFee: Long,
    val approvedFeatures: Set[Short],
    val reward: Option[Long],
    val hitSource: ByteStr,
    leasesToCancel: Map[ByteStr, Diff]
) {

  private[this] case class MicroBlockInfo(totalBlockId: BlockId, microBlock: MicroBlock) {
    def idEquals(id: ByteStr): Boolean = totalBlockId == id
  }

  private[this] case class CachedMicroDiff(diff: Diff, carryFee: Long, totalFee: Long, timestamp: Long)
  private[this] val MaxTotalDiffs = 15

  private[this] val microDiffs: MMap[BlockId, CachedMicroDiff] = MMap.empty
  private[this] val microBlocks: MList[MicroBlockInfo]         = MList.empty // fresh head

  def cancelExpiredLeases(diff: Diff): Diff =
    leasesToCancel
      .collect { case (id, ld) if diff.leaseState.getOrElse(id, true) => ld }
      .foldLeft(diff) {
        case (d, ld) =>
          Monoid.combine(d, ld)
      }

  def microBlockIds: Seq[BlockId] =
    microBlocks.map(_.totalBlockId)

  def diffFor(totalResBlockRef: BlockId): (Diff, Long, Long) = {
    val (diff, carry, totalFee) =
      if (totalResBlockRef == base.uniqueId)
        (baseBlockDiff, baseBlockCarry, baseBlockTotalFee)
      else
        internalCaches.blockDiffCache.get(
          totalResBlockRef, { () =>
            microBlocks.find(_.idEquals(totalResBlockRef)) match {
              case Some(MicroBlockInfo(_, current)) =>
                val (prevDiff, prevCarry, prevTotalFee)                   = this.diffFor(current.reference)
                val CachedMicroDiff(currDiff, currCarry, currTotalFee, _) = this.microDiffs(current.totalResBlockSig)
                (Monoid.combine(prevDiff, currDiff), prevCarry + currCarry, prevTotalFee + currTotalFee)

              case None =>
                (Diff.empty, 0L, 0L)
            }
          }
        )
    (diff, carry, totalFee)
  }

  def bestLiquidBlockId: BlockId =
    microBlocks.headOption.map(_.totalBlockId).getOrElse(base.uniqueId)

  def lastMicroBlock: Option[MicroBlock] =
    microBlocks.headOption.map(_.microBlock)

  def transactions: Seq[Transaction] =
    base.transactionData.toVector ++ microBlocks.view.map(_.microBlock.transactionData).reverse.flatten

  def bestLiquidBlock: Block =
    if (microBlocks.isEmpty)
      base
    else
      internalCaches.bestBlockCache match {
        case Some(cachedBlock) =>
          cachedBlock

        case None =>
          val block = Block.create(base, transactions, microBlocks.head.microBlock.totalResBlockSig)
          internalCaches.bestBlockCache = Some(block)
          block
      }

  def totalDiffOf(id: BlockId): Option[(Block, Diff, Long, Long, DiscardedMicroBlocks)] =
    forgeBlock(id).map {
      case (block, discarded) =>
        val (diff, carry, totalFee) = this.diffFor(id)
        (block, diff, carry, totalFee, discarded)
    }

  /** HACK: this method returns LPOS portfolio as though expired leases have already been cancelled.
    * It was added to make sure miner gets proper generating balance when scheduling next mining attempt.
    */
  def balanceDiffAt(address: Address, blockId: BlockId): Portfolio =
    cancelExpiredLeases(diffFor(blockId)._1).portfolios.getOrElse(address, Portfolio.empty)

  def bestLiquidDiffAndFees: (Diff, Long, Long) = diffFor(microBlocks.headOption.fold(base.uniqueId)(_.totalBlockId))

  def bestLiquidDiff: Diff = bestLiquidDiffAndFees._1

  def contains(blockId: BlockId): Boolean = base.uniqueId == blockId || microBlocks.exists(_.idEquals(blockId))

  def microBlock(id: BlockId): Option[MicroBlock] =
    microBlocks.find(_.idEquals(id)).map(_.microBlock)

  def bestLastBlockInfo(maxTimeStamp: Long): BlockMinerInfo = {
    val blockId = microBlocks
      .find(mi => microDiffs(mi.microBlock.totalResBlockSig).timestamp <= maxTimeStamp)
      .map(_.totalBlockId)
      .getOrElse(base.uniqueId)

    BlockMinerInfo(NxtLikeConsensusBlockData(base.header.baseTarget, base.header.generationSignature), base.header.timestamp, blockId)
  }

  def append(microBlock: MicroBlock, diff: Diff, microblockCarry: Long, microblockTotalFee: Long, timestamp: Long): BlockId = {
    val blockId = this.createBlockId(microBlock.transactionData, microBlock.totalResBlockSig)
    microDiffs.put(microBlock.totalResBlockSig, CachedMicroDiff(diff, microblockCarry, microblockTotalFee, timestamp))
    microBlocks.prepend(MicroBlockInfo(blockId, microBlock))
    internalCaches.invalidate(microBlock.totalResBlockSig)
    internalCaches.invalidate(blockId)
    blockId
  }

  def carryFee: Long =
    baseBlockCarry + microDiffs.values.map(_.carryFee).sum

  private[this] def createBlockId(transactions: Seq[Transaction], signature: ByteStr): ByteStr = {
    val newTransactions = this.transactions ++ transactions
    val transactionsRoot = com.wavesplatform.block.mkTransactionsRoot(base.header.version, newTransactions)
    val fullBlock = base.copy(transactionData = newTransactions, signature = signature, header = base.header.copy(transactionsRoot = transactionsRoot))
    fullBlock.uniqueId
  }

  private[this] def forgeBlock(blockId: BlockId): Option[(Block, DiscardedMicroBlocks)] =
    internalCaches.forgedBlockCache.get(
      blockId, { () =>
        val microBlocksAsc = microBlocks.reverse

        if (base.uniqueId == blockId) {
          Some((base, microBlocksAsc.map(_.microBlock)))
        } else if (!microBlocksAsc.exists(_.idEquals(blockId))) None
        else {
          val (accumulatedTxs, maybeFound) = microBlocksAsc.foldLeft((Vector.empty[Transaction], Option.empty[(ByteStr, DiscardedMicroBlocks)])) {
            case ((accumulated, Some((sig, discarded))), MicroBlockInfo(_, micro)) =>
              (accumulated, Some((sig, micro +: discarded)))

            case ((accumulated, None), mb) if mb.idEquals(blockId) =>
              val found = Some(mb.microBlock.totalResBlockSig -> Seq.empty[MicroBlock])
              (accumulated ++ mb.microBlock.transactionData, found)

            case ((accumulated, None), MicroBlockInfo(_, mb)) =>
              (accumulated ++ mb.transactionData, None)
          }

          maybeFound.map {
            case (sig, discarded) =>
              (Block.create(base, base.transactionData ++ accumulatedTxs, sig), discarded)
          }
        }
      }
    )

  private[this] object internalCaches {
    val blockDiffCache = CacheBuilder
      .newBuilder()
      .maximumSize(MaxTotalDiffs)
      .expireAfterWrite(10, TimeUnit.MINUTES)
      .build[BlockId, (Diff, Long, Long)]()

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

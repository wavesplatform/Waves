package com.wavesplatform.state

import com.google.common.cache.CacheBuilder
import com.wavesplatform.block
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.state.NgState.{CachedMicroDiff, MicroBlockInfo, NgStateCaches}
import com.wavesplatform.transaction.{DiscardedMicroBlocks, Transaction}

import java.util.concurrent.TimeUnit

object NgState {
  case class MicroBlockInfo(totalBlockId: BlockId, microBlock: MicroBlock) {
    def idEquals(id: ByteStr): Boolean = totalBlockId == id
  }

  case class CachedMicroDiff(diff: Diff, minerDiff: Diff, carryFee: Long, totalFee: Long, timestamp: Long)

  class NgStateCaches {
    val blockDiffCache = CacheBuilder
      .newBuilder()
      .maximumSize(NgState.MaxTotalDiffs)
      .expireAfterWrite(10, TimeUnit.MINUTES)
      .build[BlockId, (Diff, Diff, Long, Long)]()

    val forgedBlockCache = CacheBuilder
      .newBuilder()
      .maximumSize(NgState.MaxTotalDiffs)
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

  private val MaxTotalDiffs = 15
}

case class NgState(
    base: Block,
    baseBlockDiff: Diff,
    baseMinerDiff: Diff,
    baseBlockCarry: Long,
    baseBlockTotalFee: Long,
    approvedFeatures: Set[Short],
    reward: Option[Long],
    hitSource: ByteStr,
    leasesToCancel: Map[ByteStr, Diff],
    microDiffs: Map[BlockId, CachedMicroDiff] = Map.empty,
    microBlocks: List[MicroBlockInfo] = List.empty,
    internalCaches: NgStateCaches = new NgStateCaches
) {
  def cancelExpiredLeases(diff: Diff): Either[String, Diff] =
    leasesToCancel
      .collect { case (id, ld) if diff.leaseState.get(id).forall(_.isActive) => ld }
      .toList
      .foldLeft[Either[String, Diff]](Right(diff)) {
        case (Right(d1), d2) => d1.combineF(d2)
        case (r, _)          => r
      }

  def microBlockIds: Seq[BlockId] = microBlocks.map(_.totalBlockId)

  def diffFor(totalResBlockRef: BlockId): (Diff, Diff, Long, Long) = {
    if (totalResBlockRef == base.id())
      (baseBlockDiff, baseMinerDiff, baseBlockCarry, baseBlockTotalFee)
    else
      internalCaches.blockDiffCache.get(
        totalResBlockRef,
        { () =>
          microBlocks.find(_.idEquals(totalResBlockRef)) match {
            case Some(MicroBlockInfo(blockId, current)) =>
              val (prevDiff, prevMinerDiff, prevCarry, prevTotalFee)                   = this.diffFor(current.reference)
              val CachedMicroDiff(currDiff, currMinerDiff, currCarry, currTotalFee, _) = this.microDiffs(blockId)
              (
                prevDiff.combineF(currDiff).explicitGet(),
                prevMinerDiff.combineF(currMinerDiff).explicitGet(),
                prevCarry + currCarry,
                prevTotalFee + currTotalFee
              )

            case None =>
              (Diff.empty, Diff.empty, 0L, 0L)
          }
        }
      )
  }

  def bestLiquidBlockId: BlockId =
    microBlocks.headOption.fold(base.id())(_.totalBlockId)

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

  def totalDiffOf(id: BlockId): Option[(Block, Diff, Diff, Long, Long, DiscardedMicroBlocks)] =
    forgeBlock(id).map { case (block, discarded) =>
      val (diff, minerDiff, carry, totalFee) = this.diffFor(id)
      (block, diff, minerDiff, carry, totalFee, discarded)
    }

  def bestLiquidDiffAndFees: (Diff, Diff, Long, Long) = diffFor(microBlocks.headOption.fold(base.id())(_.totalBlockId))

  def bestLiquidDiff: Diff = bestLiquidDiffAndFees._1

  def allDiffs: Seq[(MicroBlock, Diff)] =
    microBlocks.toVector.map(mb => mb.microBlock -> microDiffs(mb.totalBlockId).diff).reverse

  def contains(blockId: BlockId): Boolean =
    base.id() == blockId || microBlocks.exists(_.idEquals(blockId))

  def microBlock(id: BlockId): Option[MicroBlock] =
    microBlocks.find(_.idEquals(id)).map(_.microBlock)

  def bestLastBlockInfo(maxTimeStamp: Long): BlockMinerInfo = {
    val blockId = microBlocks
      .find(mi => microDiffs(mi.totalBlockId).timestamp <= maxTimeStamp)
      .fold(base.id())(_.totalBlockId)

    BlockMinerInfo(base.header.baseTarget, base.header.generationSignature, base.header.timestamp, blockId)
  }

  def append(
      microBlock: MicroBlock,
      diff: Diff,
      minerDiff: Diff,
      microblockCarry: Long,
      microblockTotalFee: Long,
      timestamp: Long,
      totalBlockId: Option[BlockId] = None
  ): NgState = {
    val blockId = totalBlockId.getOrElse(this.createBlockId(microBlock))

    val microDiffs  = this.microDiffs + (blockId -> CachedMicroDiff(diff, minerDiff, microblockCarry, microblockTotalFee, timestamp))
    val microBlocks = MicroBlockInfo(blockId, microBlock) :: this.microBlocks
    internalCaches.invalidate(blockId)
    this.copy(microDiffs = microDiffs, microBlocks = microBlocks)
  }

  def carryFee: Long =
    baseBlockCarry + microDiffs.values.map(_.carryFee).sum

  def createBlockId(microBlock: MicroBlock): BlockId = {
    val newTransactions = this.transactions ++ microBlock.transactionData
    val fullBlock =
      base.copy(
        transactionData = newTransactions,
        signature = microBlock.totalResBlockSig,
        header = base.header.copy(transactionsRoot = createTransactionsRoot(microBlock))
      )
    fullBlock.id()
  }

  def createTransactionsRoot(microBlock: MicroBlock): ByteStr = {
    val newTransactions = this.transactions ++ microBlock.transactionData
    block.mkTransactionsRoot(base.header.version, newTransactions)
  }

  private[this] def forgeBlock(blockId: BlockId): Option[(Block, DiscardedMicroBlocks)] =
    internalCaches.forgedBlockCache.get(
      blockId,
      { () =>
        val microBlocksAsc = microBlocks.reverse

        if (base.id() == blockId) {
          Some(
            (
              base,
              microBlocksAsc.toVector.map { mb =>
                val diff = microDiffs(mb.totalBlockId).diff
                (mb.microBlock, diff)
              }
            )
          )
        } else if (!microBlocksAsc.exists(_.idEquals(blockId))) None
        else {
          val (accumulatedTxs, maybeFound) = microBlocksAsc.foldLeft((Vector.empty[Transaction], Option.empty[(ByteStr, DiscardedMicroBlocks)])) {
            case ((accumulated, Some((sig, discarded))), MicroBlockInfo(mbId, micro)) =>
              val discDiff = microDiffs(mbId).diff
              (accumulated, Some((sig, discarded :+ (micro -> discDiff))))

            case ((accumulated, None), mb) if mb.idEquals(blockId) =>
              val found = Some((mb.microBlock.totalResBlockSig, Seq.empty[(MicroBlock, Diff)]))
              (accumulated ++ mb.microBlock.transactionData, found)

            case ((accumulated, None), MicroBlockInfo(_, mb)) =>
              (accumulated ++ mb.transactionData, None)
          }

          maybeFound.map { case (sig, discarded) =>
            (Block.create(base, base.transactionData ++ accumulatedTxs, sig), discarded)
          }
        }
      }
    )
}

package com.wavesplatform.state

import cats.implicits.catsSyntaxSemigroup
import com.google.common.cache.CacheBuilder
import com.wavesplatform.block
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.NgState.{CachedMicroDiff, MicroBlockInfo, NgStateCaches}
import com.wavesplatform.state.StateSnapshot.monoid
import com.wavesplatform.transaction.{DiscardedMicroBlocks, Transaction}

import java.util.concurrent.TimeUnit

object NgState {
  case class MicroBlockInfo(totalBlockId: BlockId, microBlock: MicroBlock) {
    def idEquals(id: ByteStr): Boolean = totalBlockId == id
  }

  case class CachedMicroDiff(snapshot: StateSnapshot, carryFee: Long, totalFee: Long, timestamp: Long)

  class NgStateCaches {
    val blockDiffCache = CacheBuilder
      .newBuilder()
      .maximumSize(NgState.MaxTotalDiffs)
      .expireAfterWrite(10, TimeUnit.MINUTES)
      .build[BlockId, (StateSnapshot, Long, Long)]()

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
    baseBlockSnapshot: StateSnapshot,
    baseBlockCarry: Long,
    baseBlockTotalFee: Long,
    approvedFeatures: Set[Short],
    reward: Option[Long],
    hitSource: ByteStr,
    leasesToCancel: Map[ByteStr, StateSnapshot],
    microSnapshots: Map[BlockId, CachedMicroDiff] = Map.empty,
    microBlocks: List[MicroBlockInfo] = List.empty,
    internalCaches: NgStateCaches = new NgStateCaches
) {
  def cancelExpiredLeases(snapshot: StateSnapshot): StateSnapshot =
    leasesToCancel
      .collect { case (id, ld) if snapshot.leaseStates.get(id).forall(_.isActive) => ld }
      .toList
      .foldLeft(snapshot)(_ |+| _)

  def microBlockIds: Seq[BlockId] = microBlocks.map(_.totalBlockId)

  def snapshotFor(totalResBlockRef: BlockId): (StateSnapshot, Long, Long) = {
    val (diff, carry, totalFee) =
      if (totalResBlockRef == base.id())
        (baseBlockSnapshot, baseBlockCarry, baseBlockTotalFee)
      else
        internalCaches.blockDiffCache.get(
          totalResBlockRef,
          { () =>
            microBlocks.find(_.idEquals(totalResBlockRef)) match {
              case Some(MicroBlockInfo(blockId, current)) =>
                val (prevDiff, prevCarry, prevTotalFee)                   = this.snapshotFor(current.reference)
                val CachedMicroDiff(currDiff, currCarry, currTotalFee, _) = this.microSnapshots(blockId)
                (prevDiff |+| currDiff, prevCarry + currCarry, prevTotalFee + currTotalFee)

              case None =>
                (StateSnapshot.empty, 0L, 0L)
            }
          }
        )
    (diff, carry, totalFee)
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
          val block = Block.create(base, transactions, microBlocks.head.microBlock.totalResBlockSig, microBlocks.head.microBlock.stateHash)
          internalCaches.bestBlockCache = Some(block)
          block
      }

  def snapshotOf(id: BlockId): Option[(Block, StateSnapshot, Long, Long, DiscardedMicroBlocks)] =
    forgeBlock(id).map { case (block, discarded) =>
      val (diff, carry, totalFee) = this.snapshotFor(id)
      (block, diff, carry, totalFee, discarded)
    }

  def bestLiquidSnapshotAndFees: (StateSnapshot, Long, Long) = snapshotFor(microBlocks.headOption.fold(base.id())(_.totalBlockId))

  def bestLiquidSnapshot: StateSnapshot = bestLiquidSnapshotAndFees._1

  def allSnapshots: Seq[(MicroBlock, StateSnapshot)] =
    microBlocks.toVector.map(mb => mb.microBlock -> microSnapshots(mb.totalBlockId).snapshot).reverse

  def contains(blockId: BlockId): Boolean =
    base.id() == blockId || microBlocks.exists(_.idEquals(blockId))

  def microBlock(id: BlockId): Option[MicroBlock] =
    microBlocks.find(_.idEquals(id)).map(_.microBlock)

  def bestLastBlockInfo(maxTimeStamp: Long): BlockMinerInfo = {
    val blockId = microBlocks
      .find(mi => microSnapshots(mi.totalBlockId).timestamp <= maxTimeStamp)
      .fold(base.id())(_.totalBlockId)

    BlockMinerInfo(base.header.baseTarget, base.header.generationSignature, base.header.timestamp, blockId)
  }

  def append(
      microBlock: MicroBlock,
      snapshot: StateSnapshot,
      microblockCarry: Long,
      microblockTotalFee: Long,
      timestamp: Long,
      totalBlockId: Option[BlockId] = None
  ): NgState = {
    val blockId = totalBlockId.getOrElse(this.createBlockId(microBlock))

    val microSnapshots = this.microSnapshots + (blockId -> CachedMicroDiff(snapshot, microblockCarry, microblockTotalFee, timestamp))
    val microBlocks    = MicroBlockInfo(blockId, microBlock) :: this.microBlocks
    internalCaches.invalidate(blockId)
    this.copy(microSnapshots = microSnapshots, microBlocks = microBlocks)
  }

  def carryFee: Long =
    baseBlockCarry + microSnapshots.values.map(_.carryFee).sum

  def createBlockId(microBlock: MicroBlock): BlockId = {
    val newTransactions = this.transactions ++ microBlock.transactionData
    val fullBlock =
      base.copy(
        transactionData = newTransactions,
        signature = microBlock.totalResBlockSig,
        header = base.header.copy(transactionsRoot = createTransactionsRoot(microBlock), stateHash = microBlock.stateHash)
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
                val diff = microSnapshots(mb.totalBlockId).snapshot
                (mb.microBlock, diff)
              }
            )
          )
        } else if (!microBlocksAsc.exists(_.idEquals(blockId))) None
        else {
          val (accumulatedTxs, maybeFound) =
            microBlocksAsc.foldLeft((Vector.empty[Transaction], Option.empty[(ByteStr, Option[ByteStr], DiscardedMicroBlocks)])) {
              case ((accumulated, Some((sig, stateHash, discarded))), MicroBlockInfo(mbId, micro)) =>
                val discDiff = microSnapshots(mbId).snapshot
                (accumulated, Some((sig, stateHash, discarded :+ (micro -> discDiff))))

              case ((accumulated, None), mb) if mb.idEquals(blockId) =>
                val found = Some((mb.microBlock.totalResBlockSig, mb.microBlock.stateHash, Seq.empty[(MicroBlock, StateSnapshot)]))
                (accumulated ++ mb.microBlock.transactionData, found)

              case ((accumulated, None), MicroBlockInfo(_, mb)) =>
                (accumulated ++ mb.transactionData, None)
            }

          maybeFound.map { case (sig, stateHash, discarded) =>
            (Block.create(base, base.transactionData ++ accumulatedTxs, sig, stateHash), discarded)
          }
        }
      }
    )
}

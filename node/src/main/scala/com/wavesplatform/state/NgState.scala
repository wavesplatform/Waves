package com.wavesplatform.state

import cats.implicits.catsSyntaxSemigroup
import cats.syntax.option.*
import com.google.common.cache.{Cache, CacheBuilder}
import com.wavesplatform.block
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, FinalizationVoting, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.NgState.{BlockData, LiquidBlock, NgStateCaches}
import com.wavesplatform.state.StateSnapshot.monoid
import com.wavesplatform.transaction.{DiscardedMicroBlocks, Transaction}

import scala.collection.immutable.VectorMap
import scala.concurrent.duration.*
import scala.jdk.DurationConverters.*

object NgState {
  case class LiquidBlock(block: Block, discarded: DiscardedMicroBlocks, data: BlockData)

  case class BlockData(
      snapshot: StateSnapshot,
      carryFee: Long,
      totalFee: Long,
      liquidStateHash: ByteStr,
      finalizedHeight: Height,
      finalizationVoting: Option[FinalizationVoting]
  ) {
    def mergeToLiquid(latest: BlockData): BlockData = BlockData(
      snapshot |+| latest.snapshot,
      carryFee + latest.carryFee,
      totalFee + latest.totalFee,
      latest.liquidStateHash,
      latest.finalizedHeight,
      latest.finalizationVoting.orElse(finalizationVoting)
    )
  }

  class NgStateCaches {
    val liquidBlocks = mkCacheByBlockId[BlockData]
    val forgedBlocks = mkCacheByBlockId[Option[(Block, DiscardedMicroBlocks)]]

    @volatile
    var bestBlock = Option.empty[Block]

    def invalidate(newBlockId: BlockId): Unit = {
      forgedBlocks.invalidateAll()
      liquidBlocks.invalidate(newBlockId)
      bestBlock = None
    }

    private def mkCacheByBlockId[DataT <: Any]: Cache[BlockId, DataT] = CacheBuilder
      .newBuilder()
      .maximumSize(NgState.MaxTotalDiffs)
      .expireAfterWrite(10.minutes.toJava)
      .build[BlockId, DataT]()
  }

  private val MaxTotalDiffs = 15
}

/** @param microSnapshots Data is related to this microblock, not to a liquid block
  */
case class NgState(
    base: Block,
    baseBlockSnapshot: StateSnapshot,
    baseBlockCarry: Long,
    baseBlockTotalFee: Long,
    baseBlockComputedStateHash: ByteStr,
    approvedFeatures: Set[Short],
    reward: Option[Long],
    hitSource: ByteStr,
    leasesToCancel: Map[ByteStr, StateSnapshot],
    finalizationState: FinalizationState,
    microSnapshots: VectorMap[BlockId, (microBlock: MicroBlock, data: BlockData, receivedTimestampMs: Long)] = VectorMap.empty,
    internalCaches: NgStateCaches = new NgStateCaches
) {
  def cancelExpiredLeases(snapshot: StateSnapshot): StateSnapshot =
    leasesToCancel
      .collect { case (id, ld) if !snapshot.cancelledLeases.contains(id) => ld }
      .toList
      .foldLeft(snapshot)(_ |+| _)

  def microBlockIds: Seq[BlockId] = microSnapshots.keys.reverseIterator.toSeq

  def snapshotFor(totalResBlockRef: BlockId): BlockData =
    if (totalResBlockRef == base.id())
      BlockData(
        baseBlockSnapshot,
        baseBlockCarry,
        baseBlockTotalFee,
        baseBlockComputedStateHash,
        finalizationState.baseFinalizedHeight,
        finalizationState.accFinalizationVoting
      )
    else
      internalCaches.liquidBlocks.get(
        totalResBlockRef,
        { () =>
          val mb = microSnapshots.getOrElse(totalResBlockRef, throw new RuntimeException(s"Can't find liquid block $totalResBlockRef"))
          this.snapshotFor(mb.microBlock.reference).mergeToLiquid(mb.data)
        }
      )

  def bestLiquidBlockId: BlockId = microSnapshots.lastOption.fold(base.id())(_._1)

  def lastMicroBlock: Option[MicroBlock] = microSnapshots.lastOption.map(_._2.microBlock)

  def transactions: Seq[Transaction] = base.transactionData.toVector ++ microSnapshots.valuesIterator.flatMap(_.microBlock.transactionData)

  def bestLiquidBlock: Block = lastMicroBlock.fold(base) { lastMb =>
    internalCaches.bestBlock match {
      case Some(cachedBlock) => cachedBlock
      case None =>
        val block = Block.create(
          base,
          transactions,
          lastMb.totalResBlockSig,
          lastMb.stateHash,
          finalizationState.accFinalizationVoting
        )
        internalCaches.bestBlock = Some(block)
        block
    }
  }

  def liquidBlockOf(id: BlockId): Option[LiquidBlock] =
    forgeBlock(id).map { r =>
      LiquidBlock(r.forged, r.discarded, this.snapshotFor(id))
    }

  def bestLiquidSnapshotAndFees: (StateSnapshot, Long, Long) = {
    val s = snapshotFor(bestLiquidBlockId)
    (s.snapshot, s.carryFee, s.totalFee)
  }

  def bestLiquidSnapshot: StateSnapshot = bestLiquidSnapshotAndFees._1

  def bestLiquidComputedStateHash: ByteStr = snapshotFor(bestLiquidBlockId)._4

  def allSnapshots: Seq[(MicroBlock, StateSnapshot)] =
    microSnapshots.valuesIterator.map { mb => mb.microBlock -> mb.data.snapshot }.toVector

  def contains(blockId: BlockId): Boolean = base.id() == blockId || microSnapshots.contains(blockId)

  def microBlock(totalBlockId: BlockId): Option[MicroBlock] = microSnapshots.get(totalBlockId).map(_.microBlock)

  def bestLastBlockInfo(maxTimeStamp: Long): BlockMinerInfo = {
    val blockId = microSnapshots.keys.reverseIterator
      .collectFirst { case bestBlockId if microSnapshots(bestBlockId).receivedTimestampMs <= maxTimeStamp => bestBlockId }
      .getOrElse(base.id())

    BlockMinerInfo(base.header.baseTarget, base.header.generationSignature, base.header.timestamp, blockId)
  }

  def append(
      microBlock: MicroBlock,
      snapshot: StateSnapshot,
      microblockCarry: Long,
      microblockTotalFee: Long,
      timestamp: Long,
      liquidStateHash: ByteStr,
      totalBlockId: Option[BlockId] = None,
      updatedGeneratorSet: GeneratorSet
  ): NgState = {
    val fixedTotalBlockId = totalBlockId.getOrElse(this.createTotalBlockId(microBlock))
    val finalization      = finalizationState.append(fixedTotalBlockId, microBlock.finalizationVoting, updatedGeneratorSet)

    val microSnapshots = this.microSnapshots.updated(
      fixedTotalBlockId,
      (
        microBlock,
        BlockData(snapshot, microblockCarry, microblockTotalFee, liquidStateHash, finalization.height, finalization.accVoting),
        timestamp
      )
    )

    internalCaches.invalidate(fixedTotalBlockId)
    this.copy(
      microSnapshots = microSnapshots,
      finalizationState = finalization.updatedState
    )
  }

  def carryFee: Long = baseBlockCarry + microSnapshots.valuesIterator.map(_.data.carryFee).sum

  def createTotalBlockId(lastMicroBlock: MicroBlock): BlockId = {
    val newTransactions = this.transactions ++ lastMicroBlock.transactionData

    val fullBlock = base.copy(
      transactionData = newTransactions,
      signature = lastMicroBlock.totalResBlockSig,
      header = base.header.copy(
        transactionsRoot = createTransactionsRoot(lastMicroBlock),
        stateHash = lastMicroBlock.stateHash,
        finalizationVoting = FinalizationVoting.combine(finalizationState.accFinalizationVoting, lastMicroBlock.finalizationVoting)
      )
    )
    fullBlock.id()
  }

  def createTransactionsRoot(microBlock: MicroBlock): ByteStr = {
    val newTransactions = this.transactions ++ microBlock.transactionData
    block.mkTransactionsRoot(base.header.version, newTransactions)
  }

  private def forgeBlock(blockId: BlockId): Option[(forged: Block, discarded: DiscardedMicroBlocks)] =
    internalCaches.forgedBlocks.get(
      blockId,
      { () =>
        if (base.id() == blockId)
          (
            base,
            microSnapshots.valuesIterator.map { mb => (mb.microBlock, mb.data.snapshot) }.toVector
          ).some
        else if (!microSnapshots.contains(blockId)) None
        else {
          val init = (
            base.transactionData,
            Option.empty[FinalizationVoting],
            Option.empty[(sig: ByteStr, stateHash: Option[ByteStr], discarded: DiscardedMicroBlocks)]
          )
          val (txs, voting, maybeFound) = microSnapshots.foldLeft(init) {
            case ((txs, voting, Some(found)), (_, mb)) => // Already found
              val discDiff = mb.data.snapshot
              (txs, voting, (found.sig, found.stateHash, found.discarded.appended(mb.microBlock -> discDiff)).some)

            case ((txs, _, None), (totalBlockId, mb)) if totalBlockId == blockId => // Found now
              val found = (mb.microBlock.totalResBlockSig, mb.microBlock.stateHash, Seq.empty[(MicroBlock, StateSnapshot)]).some
              (txs ++ mb.microBlock.transactionData, mb.data.finalizationVoting, found) // finalizationVoting already combined

            case ((txs, _, None), (_, mb)) => // Not yet found
              (txs ++ mb.microBlock.transactionData, None, None)
          }

          maybeFound.map { found =>
            (Block.create(base, txs, found.sig, found.stateHash, voting), found.discarded)
          }
        }
      }
    )
}

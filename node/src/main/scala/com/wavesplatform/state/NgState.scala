package com.wavesplatform.state

import cats.implicits.catsSyntaxSemigroup
import com.google.common.cache.CacheBuilder
import com.wavesplatform.block
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, FinalizationVoting, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.NgState.{BlockData, LiquidBlock, MicroBlockInfo, NgStateCaches}
import com.wavesplatform.state.StateSnapshot.monoid
import com.wavesplatform.transaction.{DiscardedMicroBlocks, Transaction}

import java.util.concurrent.TimeUnit

object NgState {
  case class MicroBlockInfo(totalBlockId: BlockId, microBlock: MicroBlock) {
    def idEquals(id: ByteStr): Boolean = totalBlockId == id
  }

  case class LiquidBlock(
      block: Block,
      discarded: DiscardedMicroBlocks,
      liquid: BlockData
  )

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
    val liquidBlocks = CacheBuilder
      .newBuilder()
      .maximumSize(NgState.MaxTotalDiffs)
      .expireAfterWrite(10, TimeUnit.MINUTES)
      .build[BlockId, BlockData]()

    val forgedBlocks = CacheBuilder
      .newBuilder()
      .maximumSize(NgState.MaxTotalDiffs)
      .expireAfterWrite(10, TimeUnit.MINUTES)
      .build[BlockId, Option[(Block, DiscardedMicroBlocks)]]()

    @volatile
    var bestBlock = Option.empty[Block]

    def invalidate(newBlockId: BlockId): Unit = {
      forgedBlocks.invalidateAll()
      liquidBlocks.invalidate(newBlockId)
      bestBlock = None
    }
  }

  private val MaxTotalDiffs = 15
}

/** @param microSnapshots Contains data related to this microblock
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
    microSnapshots: Map[BlockId, (mb: BlockData, receivedTimestampMs: Long)] = Map.empty,
    microBlocks: List[MicroBlockInfo] = List.empty, // Recent in the head
    internalCaches: NgStateCaches = new NgStateCaches
) {
  def cancelExpiredLeases(snapshot: StateSnapshot): StateSnapshot =
    leasesToCancel
      .collect { case (id, ld) if !snapshot.cancelledLeases.contains(id) => ld }
      .toList
      .foldLeft(snapshot)(_ |+| _)

  def microBlockIds: Seq[BlockId] = microBlocks.map(_.totalBlockId)

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
          microBlocks.find(_.idEquals(totalResBlockRef)) match {
            case Some(MicroBlockInfo(blockId, current)) => this.snapshotFor(current.reference).mergeToLiquid(microSnapshots(blockId).mb)
            case None                                   => throw new RuntimeException(s"Can't find liquid block $totalResBlockRef")
          }
        }
      )

  def bestLiquidBlockId: BlockId = microBlocks.headOption.fold(base.id())(_.totalBlockId)

  def lastMicroBlock: Option[MicroBlock] = microBlocks.headOption.map(_.microBlock)

  def transactions: Seq[Transaction] = base.transactionData.toVector ++ microBlocks.view.map(_.microBlock.transactionData).reverse.flatten

  def bestLiquidBlock: Block = microBlocks.headOption.fold(base) { last =>
    internalCaches.bestBlock match {
      case Some(cachedBlock) => cachedBlock
      case None =>
        val block = Block.create(
          base,
          transactions,
          last.microBlock.totalResBlockSig,
          last.microBlock.stateHash,
          finalizationState.accFinalizationVoting
        )
        internalCaches.bestBlock = Some(block)
        block
    }
  }

  def liquidBlockOf(id: BlockId): Option[LiquidBlock] =
    forgeBlock(id).map { case (block, discarded) =>
      LiquidBlock(block, discarded, this.snapshotFor(id))
    }

  def bestLiquidSnapshotAndFees: (StateSnapshot, Long, Long) = {
    val s = snapshotFor(microBlocks.headOption.fold(base.id())(_.totalBlockId))
    (s.snapshot, s.carryFee, s.totalFee)
  }

  def bestLiquidSnapshot: StateSnapshot = bestLiquidSnapshotAndFees._1

  def bestLiquidComputedStateHash: ByteStr = snapshotFor(microBlocks.headOption.fold(base.id())(_.totalBlockId))._4

  def allSnapshots: Seq[(MicroBlock, StateSnapshot)] =
    microBlocks.toVector.map(mb => mb.microBlock -> microSnapshots(mb.totalBlockId).mb.snapshot).reverse

  def contains(blockId: BlockId): Boolean = base.id() == blockId || microBlocks.exists(_.idEquals(blockId))

  def microBlock(totalBlockId: BlockId): Option[MicroBlock] = microBlocks.find(_.idEquals(totalBlockId)).map(_.microBlock)

  def bestLastBlockInfo(maxTimeStamp: Long): BlockMinerInfo = {
    val blockId = microBlocks
      .find(mi => microSnapshots(mi.totalBlockId).receivedTimestampMs <= maxTimeStamp)
      .fold(base.id())(_.totalBlockId)

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
        BlockData(snapshot, microblockCarry, microblockTotalFee, liquidStateHash, finalization.height, finalization.accVoting),
        timestamp
      )
    )
    val microBlocks = MicroBlockInfo(fixedTotalBlockId, microBlock) :: this.microBlocks

    internalCaches.invalidate(fixedTotalBlockId)
    this.copy(
      microSnapshots = microSnapshots,
      microBlocks = microBlocks,
      finalizationState = finalization.updatedState
    )
  }

  def carryFee: Long = baseBlockCarry + microSnapshots.values.map(_.mb.carryFee).sum

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

  private def forgeBlock(blockId: BlockId): Option[(Block, DiscardedMicroBlocks)] =
    internalCaches.forgedBlocks.get(
      blockId,
      { () =>
        val microBlocksAsc = microBlocks.reverse

        if (base.id() == blockId)
          Some(
            (
              base,
              microBlocksAsc.toVector.map { mb =>
                val diff = microSnapshots(mb.totalBlockId).mb.snapshot
                (mb.microBlock, diff)
              }
            )
          )
        else if (!microBlocksAsc.exists(_.idEquals(blockId))) None
        else {
          val init = (
            base.transactionData,
            base.header.finalizationVoting,
            Option.empty[(sig: ByteStr, stateHash: Option[ByteStr], discarded: DiscardedMicroBlocks)]
          )
          val (txs, voting, maybeFound) = microBlocksAsc.foldLeft(init) {
            case ((txs, voting, Some(found)), MicroBlockInfo(mbId, mb)) =>
              val discDiff = microSnapshots(mbId).mb.snapshot
              (txs, voting, Some((found.sig, found.stateHash, found.discarded :+ (mb -> discDiff))))

            case ((txs, voting, None), mb) if mb.idEquals(blockId) =>
              val found = Some((mb.microBlock.totalResBlockSig, mb.microBlock.stateHash, Seq.empty[(MicroBlock, StateSnapshot)]))
              (txs ++ mb.microBlock.transactionData, FinalizationVoting.combine(voting, mb.microBlock.finalizationVoting), found)

            case ((txs, voting, None), MicroBlockInfo(_, mb)) =>
              (txs ++ mb.transactionData, FinalizationVoting.combine(voting, mb.finalizationVoting), None)
          }

          maybeFound.map { found =>
            (Block.create(base, txs, found.sig, found.stateHash, voting), found.discarded)
          }
        }
      }
    )
}

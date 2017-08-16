package com.wavesplatform.state2

import java.util.concurrent.locks.ReentrantReadWriteLock

import cats._
import cats.implicits._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.BlockchainUpdaterImpl._
import com.wavesplatform.state2.diffs.BlockDiffer
import com.wavesplatform.state2.reader.CompositeStateReader.composite
import com.wavesplatform.state2.reader.StateReader
import scorex.block.Block.BlockId
import scorex.block.{Block, MicroBlock}
import scorex.transaction.ValidationError.GenericError
import scorex.transaction._
import scorex.utils.ScorexLogging

class BlockchainUpdaterImpl private(persisted: StateWriter with StateReader,
                                    settings: FunctionalitySettings,
                                    minimumInMemoryDiffSize: Int,
                                    ngHistoryWriter: NgHistoryWriter,
                                    val synchronizationToken: ReentrantReadWriteLock) extends BlockchainUpdater with ScorexLogging with Instrumented {

  private val topMemoryDiff = Synchronized(Monoid[BlockDiff].empty)
  private val bottomMemoryDiff = Synchronized(Monoid[BlockDiff].empty)
  private val liquidBlockCandidatesDiff = Synchronized(Map.empty[BlockId, BlockDiff])

  private def unsafeDiffByRange(state: StateReader, from: Int, to: Int): BlockDiff = {
    val blocks = measureLog(s"Reading blocks from $from up to $to") {
      Range(from, to).map(ngHistoryWriter.blockBytes).par.map(b => Block.parseBytes(b.get).get).seq
    }
    measureLog(s"Building diff from $from up to $to") {
      BlockDiffer.unsafeDiffMany(settings, state, ngHistoryWriter.blockAt(from - 1).map(_.timestamp))(blocks)
    }
  }

  private def logHeights(prefix: String): Unit = read { implicit l =>
    log.info(s"$prefix, total blocks: ${ngHistoryWriter.height()}, persisted: ${persisted.height}, " +
      s"topMemDiff: ${topMemoryDiff().heightDiff}, bottomMemDiff: ${bottomMemoryDiff().heightDiff}")
  }

  private def currentPersistedBlocksState: StateReader = read { implicit l =>
    composite(composite(persisted, () => bottomMemoryDiff()), () => topMemoryDiff())
  }

  private def bestLiquidDiff(): BlockDiff = read { implicit l =>
    ngHistoryWriter.bestLiquidBlock()
      .map(_.uniqueId)
      .map(liquidBlockCandidatesDiff().get(_).get)
      .map(_.copy(heightDiff = 1))
      .orEmpty
  }

  def bestLiquidState: StateReader = read { implicit l =>
    composite(currentPersistedBlocksState, () => bestLiquidDiff())
  }

  private def updatePersistedAndInMemory(): Unit = write { implicit l =>
    logHeights("State rebuild started")
    val persistFrom = persisted.height + 1
    val persistUpTo = ngHistoryWriter.height - minimumInMemoryDiffSize + 1

    ranges(persistFrom, persistUpTo, minimumInMemoryDiffSize).foreach { case (head, last) =>
      val diffToBePersisted = unsafeDiffByRange(persisted, head, last)
      persisted.applyBlockDiff(diffToBePersisted)
    }

    bottomMemoryDiff.set(unsafeDiffByRange(persisted, persisted.height + 1, ngHistoryWriter.height() + (if (ngHistoryWriter.baseBlock().isDefined) 0 else 1)))
    topMemoryDiff.set(BlockDiff.empty)
    logHeights("State rebuild finished")
  }

  override def processBlock(block: Block): Either[ValidationError, DiscardedTransactions] = write { implicit l =>
    if (topMemoryDiff().heightDiff >= minimumInMemoryDiffSize) {
      persisted.applyBlockDiff(bottomMemoryDiff())
      bottomMemoryDiff.set(topMemoryDiff())
      topMemoryDiff.set(BlockDiff.empty)
    }
    (liquidBlockCandidatesDiff().get(block.reference) match {
      case Some(referencedLiquidDiff) =>
        val asFirmBlock = referencedLiquidDiff.copy(heightDiff = 1)
        ngHistoryWriter.appendBlock(block)(BlockDiffer.fromBlock(settings,
          composite(currentPersistedBlocksState, () => asFirmBlock),
          ngHistoryWriter.bestLiquidBlock().map(_.timestamp), block)).map { case ((newBlockDiff, discraded)) =>
          topMemoryDiff.set(Monoid.combine(topMemoryDiff(), asFirmBlock))
          liquidBlockCandidatesDiff.set(Map(block.uniqueId -> newBlockDiff))
          discraded
        }
      case None =>
        ngHistoryWriter.appendBlock(block)(BlockDiffer.fromBlock(
          settings, currentPersistedBlocksState, ngHistoryWriter.lastBlockTimestamp(), block)).map { case ((newBlockDiff, discraded)) =>
          liquidBlockCandidatesDiff.set(Map(block.uniqueId -> newBlockDiff))
          discraded
        }
    }).map(discacrded => {
      log.info(
        s"""Block ${block.uniqueId} -> ${trim(block.reference)} appended.
           | -- New height: ${ngHistoryWriter.height()}, transactions: ${block.transactionData.size})""".stripMargin)
      discacrded
    })
  }

  override def removeAfter(blockId: ByteStr): Either[ValidationError, Seq[Transaction]] = write { implicit l =>
    ngHistoryWriter.heightOf(blockId) match {
      case Some(height) if height == ngHistoryWriter.height() =>
        log.trace("No rollback necessary")
        Right(Seq.empty)
      case Some(height) =>
        logHeights(s"Rollback to h=$height started")
        val discardedTransactions = Seq.newBuilder[Transaction]
        if (ngHistoryWriter.height > height) {
          liquidBlockCandidatesDiff.set(Map.empty)
        }
        while (ngHistoryWriter.height > height) {
          val transactions = ngHistoryWriter.discardBlock()
          log.trace(s"Collecting ${transactions.size} discarded transactions.")
          discardedTransactions ++= transactions
        }
        if (height < persisted.height) {
          log.info(s"Rollback to h=$height requested. Persisted height=${persisted.height}, will drop state and reapply blockchain now")
          persisted.clear()
          updatePersistedAndInMemory()
        } else {
          if (bestLiquidState.height != height) {
            val persistedPlusBottomHeight = persisted.height + bottomMemoryDiff().heightDiff
            if (height > persistedPlusBottomHeight) {
              val newTopDiff = unsafeDiffByRange(composite(persisted, () => bottomMemoryDiff()), persistedPlusBottomHeight + 1, height + 1)
              topMemoryDiff.set(newTopDiff)
            } else {
              topMemoryDiff.set(BlockDiff.empty)
              if (height < persistedPlusBottomHeight)
                bottomMemoryDiff.set(unsafeDiffByRange(persisted, persisted.height + 1, height + 1))
            }
          }
        }
        logHeights(s"Rollback to h=$height completed:")
        Right(discardedTransactions.result())
      case None =>
        log.warn(s"removeAfter nonexistent block $blockId")
        Left(GenericError(s"Failed to rollback to nonexistent block $blockId"))
    }
  }

  override def processMicroBlock(microBlock: MicroBlock): Either[ValidationError, Unit] = write { implicit l =>
    val bld = bestLiquidDiff()
    ngHistoryWriter.appendMicroBlock(microBlock)(ts =>
      BlockDiffer.fromMicroBlock(settings,
        composite(currentPersistedBlocksState, () => bestLiquidDiff().copy(snapshots = Map.empty)),
        ngHistoryWriter.parent(ngHistoryWriter.lastBlock.get).map(_.timestamp), microBlock, ts))
      .map(microBlockDiff => {
        liquidBlockCandidatesDiff.set(liquidBlockCandidatesDiff() + (microBlock.totalResBlockSig -> Monoid.combine(bld, microBlockDiff)))
        log.info(s"MicroBlock ${trim(microBlock.totalResBlockSig)}~>${trim(microBlock.prevResBlockSig)} appended. " +
          s" -- with ${microBlock.transactionData.size} transactions")
      })
  }
}

object BlockchainUpdaterImpl {
  def apply(
               persistedState: StateWriter with StateReader,
               history: NgHistoryWriter,
               functionalitySettings: FunctionalitySettings,
               minimumInMemoryDiffSize: Int,
               synchronizationToken: ReentrantReadWriteLock): BlockchainUpdaterImpl = {
    val blockchainUpdater =
      new BlockchainUpdaterImpl(persistedState, functionalitySettings, minimumInMemoryDiffSize, history, synchronizationToken)
    blockchainUpdater.logHeights("Constructing BlockchainUpdaterImpl")
    blockchainUpdater.updatePersistedAndInMemory()
    blockchainUpdater
  }

  def ranges(from: Int, to: Int, by: Int): Stream[(Int, Int)] =
    if (from + by < to)
      (from, from + by) #:: ranges(from + by, to, by)
    else
      (from, to) #:: Stream.empty[(Int, Int)]
}

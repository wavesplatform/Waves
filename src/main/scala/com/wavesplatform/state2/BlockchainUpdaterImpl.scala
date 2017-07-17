package com.wavesplatform.state2

import java.util.concurrent.locks.ReentrantReadWriteLock

import cats._
import cats.data._
import cats.implicits._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.BlockchainUpdaterImpl._
import com.wavesplatform.state2.StateWriterImpl._
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
                                    val synchronizationToken: ReentrantReadWriteLock) extends BlockchainUpdater with ScorexLogging {

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
    log.info(s"$prefix Total blocks: ${ngHistoryWriter.height()}, persisted: ${persisted.height}, " +
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
    logHeights("State rebuild started:")
    val persistFrom = persisted.height + 1
    val persistUpTo = ngHistoryWriter.height - minimumInMemoryDiffSize + 1

    ranges(persistFrom, persistUpTo, minimumInMemoryDiffSize).foreach { case (head, last) =>
      val diffToBePersisted = unsafeDiffByRange(persisted, head, last)
      persisted.applyBlockDiff(diffToBePersisted)
    }

    bottomMemoryDiff.set(unsafeDiffByRange(persisted, persisted.height + 1, ngHistoryWriter.height() + (if (ngHistoryWriter.liquidBlockExists()) 0 else 1)))
    topMemoryDiff.set(BlockDiff.empty)
    logHeights("State rebuild finished:")
  }

  override def processBlock(block: Block): Either[ValidationError, Unit] = write { implicit l =>
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
          ngHistoryWriter.bestLiquidBlock().map(_.timestamp))(block)).map { newBlockDiff =>
          topMemoryDiff.set(Monoid.combine(topMemoryDiff(), asFirmBlock))
          liquidBlockCandidatesDiff.set(Map(block.uniqueId -> newBlockDiff))
        }
      case None =>
        ngHistoryWriter.appendBlock(block)(BlockDiffer.fromBlock(
          settings, currentPersistedBlocksState, ngHistoryWriter.lastBlock.map(_.timestamp))(block)).map { newBlockDiff =>
          liquidBlockCandidatesDiff.set(Map(block.uniqueId -> newBlockDiff))
        }
    }).map(_ => log.info( s"""Block ${block.uniqueId} appended. New height: ${ngHistoryWriter.height()}, new score: ${ngHistoryWriter.score()})"""))
  }

  override def removeAfter(blockId: ByteStr): Either[ValidationError, Seq[Transaction]] = write { implicit l =>
    ngHistoryWriter.heightOf(blockId) match {
      case Some(height) =>
        logHeights(s"Rollback to height $height started:")
        val discardedTransactions = Seq.newBuilder[Transaction]
        while (ngHistoryWriter.height > height) {
          val transactions = ngHistoryWriter.discardBlock()
          log.trace(s"Collecting ${transactions.size} discarded transactions: $transactions")
          discardedTransactions ++= transactions
        }
        if (height < persisted.height) {
          log.warn(s"Rollback to h=$height requested. Persisted height=${persisted.height}, will drop state and reapply blockchain now")
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
        logHeights(s"Rollback to height $height completed:")
        Right(discardedTransactions.result())
      case None =>
        log.warn(s"removeAfter non-existing block $blockId")
        Left(GenericError(s"Failed to rollback to non existing block $blockId"))
    }
  }

  override def processMicroBlock(microBlock: MicroBlock): Either[ValidationError, Unit] = write { implicit l =>
    ngHistoryWriter.appendMicroBlock(microBlock) {
      val prevTotal = ngHistoryWriter.forgeBlock(microBlock.prevResBlockSig).get
      val newTotal = prevTotal.copy(
        signerData = prevTotal.signerData.copy(signature = microBlock.totalResBlockSig),
        transactionData = prevTotal.transactionData ++ microBlock.transactionData)
      BlockDiffer.fromLiquidBlock(settings, currentPersistedBlocksState, ngHistoryWriter.parent(ngHistoryWriter.lastBlock.get).map(_.timestamp))(newTotal)
        .map(newTotalDiff => liquidBlockCandidatesDiff.set(liquidBlockCandidatesDiff() + (microBlock.totalResBlockSig -> newTotalDiff)))
    }.map(_ => log.info(s"MicroBlock ${trim(microBlock.totalResBlockSig)}~>${trim(microBlock.prevResBlockSig)} " +
      s"with ${microBlock.transactionData.size} transactions appended"))
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
    blockchainUpdater.logHeights("Constructing BlockchainUpdaterImpl:")
    blockchainUpdater.updatePersistedAndInMemory()
    blockchainUpdater
  }

  def ranges(from: Int, to: Int, by: Int): List[(Int, Int)] =
    if (from + by < to)
      (from, from + by) +: ranges(from + by, to, by)
    else List((from, to))

}

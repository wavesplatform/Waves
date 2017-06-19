package com.wavesplatform.state2

import java.util.concurrent.locks.ReentrantReadWriteLock

import cats._
import cats.implicits._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.BlockchainUpdaterImpl._
import com.wavesplatform.state2.StateWriterImpl._
import com.wavesplatform.state2.diffs.BlockDiffer
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import scorex.block.Block.BlockId
import scorex.block.{Block, MicroBlock}
import scorex.transaction._
import scorex.utils.ScorexLogging

class BlockchainUpdaterImpl private(persisted: StateWriter with StateReader,
                                    settings: FunctionalitySettings,
                                    minimumInMemoryDiffSize: Int,
                                    ngHistoryWriter: NgHistoryWriter,
                                    val synchronizationToken: ReentrantReadWriteLock) extends BlockchainUpdater with ScorexLogging {

  private val MaxInMemDiffHeight = minimumInMemoryDiffSize * 2

  private val inMemoryDiff = Synchronized(Monoid[BlockDiff].empty)
  private val liquidBlockCandidatesDiff = Synchronized(Map.empty[BlockId, BlockDiff])

  private def unsafeDiffAgainstPersistedByRange(from: Int, to: Int): BlockDiff = {
    val blocks = measureLog(s"Reading blocks from $from up to $to") {
      Range(from, to).map(ngHistoryWriter.blockBytes).par.map(b => Block.parseBytes(b.get).get).seq
    }
    measureLog(s"Building diff from $from up to $to") {
      BlockDiffer.unsafeDiffMany(settings, persisted)(blocks)
    }
  }

  private def logHeights(prefix: String): Unit = read { implicit l =>
    log.info(s"$prefix Total blocks: ${ngHistoryWriter.height()}, persisted: ${persisted.height}, imMemDiff: ${inMemoryDiff().heightDiff}")
  }

  private def currentPersistedBlocksState: StateReader = read { implicit l =>
    new CompositeStateReader.Proxy(persisted, () => inMemoryDiff())
  }

  def bestLiquidState: StateReader = read { implicit l =>
    val bestLiquidDiff = ngHistoryWriter.bestLiquidBlock()
      .map(_.uniqueId)
      .flatMap(liquidBlockCandidatesDiff().get)
      .orEmpty

    new CompositeStateReader.Proxy(persisted,
      () => Monoid.combine(inMemoryDiff(), bestLiquidDiff))
  }

  private def updatePersistedAndInMemory(): Unit = write { implicit l =>
    logHeights("State rebuild started:")
    val persistFrom = persisted.height + 1
    val persistUpTo = ngHistoryWriter.height - minimumInMemoryDiffSize + 1

    ranges(persistFrom, persistUpTo, minimumInMemoryDiffSize).foreach { case (head, last) =>
      val diffToBePersisted = unsafeDiffAgainstPersistedByRange(head, last)
      persisted.applyBlockDiff(diffToBePersisted)
    }

    inMemoryDiff.set(unsafeDiffAgainstPersistedByRange(persisted.height + 1, ngHistoryWriter.height() + 1))
    logHeights("State rebuild finished:")
  }

  override def processBlock(block: Block): Either[ValidationError, Unit] = write { implicit l =>
    if (inMemoryDiff().heightDiff >= MaxInMemDiffHeight) {
      updatePersistedAndInMemory()
    }

    liquidBlockCandidatesDiff().get(block.uniqueId) match {
      case Some(referencedLiquidDiff) => for {
        newBlockDiff <- BlockDiffer.fromBlock(settings, new CompositeStateReader.Proxy(currentPersistedBlocksState, () => referencedLiquidDiff))(block)
        _ <- ngHistoryWriter.appendBlock(block)
      } yield {
        inMemoryDiff.set(Monoid.combine(inMemoryDiff(), referencedLiquidDiff))
        liquidBlockCandidatesDiff.set(Map(block.uniqueId -> newBlockDiff))
      }
      case None => for {
        newBlockDiff <- BlockDiffer.fromBlock(settings, currentPersistedBlocksState)(block)
        _ <- ngHistoryWriter.appendBlock(block)
      } yield {
        liquidBlockCandidatesDiff.set(Map(block.uniqueId -> newBlockDiff))
      }
    }
  }

  override def removeAfter(blockId: ByteStr): Boolean = write { implicit l =>
    ngHistoryWriter.heightOf(blockId) match {
      case Some(height) =>
        logHeights(s"Rollback to height $height started:")
        while (ngHistoryWriter.height > height) {
          ngHistoryWriter.discardBlock()
        }
        if (height < persisted.height) {
          log.warn(s"Rollback to h=$height requested. Persisted height=${persisted.height}, will drop state and reapply blockchain now")
          persisted.clear()
          updatePersistedAndInMemory()
        } else {
          if (bestLiquidState.height != height) {
            inMemoryDiff.set(unsafeDiffAgainstPersistedByRange(persisted.height + 1, height + 1))
          }
        }
        logHeights(s"Rollback to height $height completed:")
        true
      case None =>
        log.warn(s"removeAfter non-existing block $blockId")
        false
    }
  }

  override def processMicroBlock(microBlock: MicroBlock): Either[ValidationError, Unit] = write { implicit l =>
    ???
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

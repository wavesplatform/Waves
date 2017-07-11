package com.wavesplatform.state2

import java.util.concurrent.locks.ReentrantReadWriteLock

import cats._
import cats.implicits._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.BlockchainUpdaterImpl._
import com.wavesplatform.state2.StateWriterImpl._
import com.wavesplatform.state2.diffs.BlockDiffer
import com.wavesplatform.state2.reader.CompositeStateReader.proxy
import com.wavesplatform.state2.reader.StateReader
import scorex.block.Block.BlockId
import scorex.block.{Block}
import scorex.transaction.ValidationError.GenericError
import scorex.transaction._
import scorex.utils.ScorexLogging

class BlockchainUpdaterImpl private(persisted: StateWriter with StateReader,
                                    settings: FunctionalitySettings,
                                    minimumInMemoryDiffSize: Int,
                                    historyWriter: HistoryWriter,
                                    val synchronizationToken: ReentrantReadWriteLock) extends BlockchainUpdater with ScorexLogging {

  private val MaxInMemDiffHeight = minimumInMemoryDiffSize * 2

  private val inMemoryDiff = Synchronized(Monoid[BlockDiff].empty)

  private def unsafeDiffAgainstPersistedByRange(from: Int, to: Int): BlockDiff = {
    val blocks = measureLog(s"Reading blocks from $from up to $to") {
      Range(from, to).map(historyWriter.blockBytes).par.map(b => Block.parseBytes(b.get).get).seq
    }
    measureLog(s"Building diff from $from up to $to") {
      BlockDiffer.unsafeDiffMany(settings, persisted)(blocks)
    }
  }

  private def logHeights(prefix: String): Unit = read { implicit l =>
    log.info(s"$prefix Total blocks: ${historyWriter.height()}, persisted: ${persisted.height}, imMemDiff: ${inMemoryDiff().heightDiff}")
  }

  def currentPersistedBlocksState: StateReader = read { implicit l =>
    proxy(persisted, () => inMemoryDiff())
  }

  private def updatePersistedAndInMemory(): Unit = write { implicit l =>
    logHeights("State rebuild started:")
    val persistFrom = persisted.height + 1
    val persistUpTo = historyWriter.height - minimumInMemoryDiffSize + 1

    ranges(persistFrom, persistUpTo, minimumInMemoryDiffSize).foreach { case (head, last) =>
      val diffToBePersisted = unsafeDiffAgainstPersistedByRange(head, last)
      persisted.applyBlockDiff(diffToBePersisted)
    }

    inMemoryDiff.set(unsafeDiffAgainstPersistedByRange(persisted.height + 1, historyWriter.height() + 1))
    logHeights("State rebuild finished:")
  }

  override def processBlock(block: Block): Either[ValidationError, Unit] = write { implicit l =>
    if (inMemoryDiff().heightDiff >= MaxInMemDiffHeight) {
      updatePersistedAndInMemory()
    }
    historyWriter.appendBlock(block)(BlockDiffer.fromBlock(settings, currentPersistedBlocksState)(block)).map { newBlockDiff =>
      inMemoryDiff.set(Monoid[BlockDiff].combine(inMemoryDiff(), newBlockDiff))
    }.map(_ => log.info( s"""Block ${block.uniqueId} appended. New height: ${historyWriter.height()}, new score: ${historyWriter.score()})"""))
  }

  override def removeAfter(blockId: ByteStr): Either[ValidationError, BigInt] = write { implicit l =>
    historyWriter.heightOf(blockId) match {
      case Some(height) =>
        logHeights(s"Rollback to height $height started:")
        while (historyWriter.height > height) {
          historyWriter.discardBlock()
        }
        if (height < persisted.height) {
          log.warn(s"Rollback to h=$height requested. Persisted height=${persisted.height}, will drop state and reapply blockchain now")
          persisted.clear()
          updatePersistedAndInMemory()
        } else {
          if (currentPersistedBlocksState.height != height) {
            inMemoryDiff.set(unsafeDiffAgainstPersistedByRange(persisted.height + 1, height + 1))
          }
        }
        logHeights(s"Rollback to height $height completed:")
        Right(historyWriter.score())
      case None =>
        log.warn(s"removeAfter non-existing block $blockId")
        Left(GenericError(s"Failed to rollback to non existing block $blockId"))
    }
  }
}

object BlockchainUpdaterImpl {
  def apply(
               persistedState: StateWriter with StateReader,
               history: HistoryWriter,
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

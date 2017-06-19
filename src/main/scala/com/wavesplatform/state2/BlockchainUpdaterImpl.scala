package com.wavesplatform.state2

import java.util.concurrent.locks.ReentrantReadWriteLock

import cats.kernel.Monoid
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.BlockchainUpdaterImpl._
import com.wavesplatform.state2.StateWriterImpl._
import com.wavesplatform.state2.diffs.BlockDiffer
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import scorex.block.{Block, MicroBlock}
import scorex.transaction._
import scorex.utils.ScorexLogging

class BlockchainUpdaterImpl private(persisted: StateWriter with StateReader,
                                    settings: FunctionalitySettings,
                                    minimumInMemoryDiffSize: Int,
                                    bc: HistoryWriter with History,
                                    val synchronizationToken: ReentrantReadWriteLock) extends BlockchainUpdater with ScorexLogging {

  private val MaxInMemDiffHeight = minimumInMemoryDiffSize * 2

  private val inMemoryDiff = Synchronized(Monoid[BlockDiff].empty)

  private def unsafeDiffAgainstPersistedByRange(from: Int, to: Int): BlockDiff =  {
      val blocks = measureLog(s"Reading blocks from $from up to $to") {
        Range(from, to).map(bc.blockBytes).par.map(b => Block.parseBytes(b.get).get).seq
      }
      measureLog(s"Building diff from $from up to $to") {
        BlockDiffer.unsafeDiffMany(settings)(persisted, blocks)
      }
  }

  private def logHeights(prefix: String): Unit = read { implicit l =>
    log.info(s"$prefix Total blocks: ${bc.height()}, persisted: ${persisted.height}, imMemDiff: ${inMemoryDiff().heightDiff}")
  }

  def currentState: StateReader = read { implicit l => new CompositeStateReader.Proxy(persisted, () => inMemoryDiff()) }

  private def updatePersistedAndInMemory(): Unit = write { implicit l =>
    logHeights("State rebuild started:")
    val persistFrom = persisted.height + 1
    val persistUpTo = bc.height - minimumInMemoryDiffSize + 1

    ranges(persistFrom, persistUpTo, minimumInMemoryDiffSize).foreach { case (head, last) =>
      val diffToBePersisted = unsafeDiffAgainstPersistedByRange(head, last)
      persisted.applyBlockDiff(diffToBePersisted)
    }

    inMemoryDiff.set(unsafeDiffAgainstPersistedByRange(persisted.height + 1, bc.height() + 1))
    logHeights("State rebuild finished:")
  }

  override def processBlock(block: Block): Either[ValidationError, Unit] = write { implicit l =>
    if (inMemoryDiff().heightDiff >= MaxInMemDiffHeight) {
      updatePersistedAndInMemory()
    }
    for {
      blockDiff <- BlockDiffer(settings)(currentState, block)
      _ <- bc.appendBlock(block)
    } yield {
      log.info(s"""Block ${block.encodedId} appended. New height: ${bc.height()}, new score: ${bc.score()})""")
      inMemoryDiff.set(Monoid[BlockDiff].combine(inMemoryDiff(), blockDiff))
    }
  }

  override def removeAfter(blockId: ByteStr): Boolean = write { implicit l =>
    bc.heightOf(blockId) match {
      case Some(height) =>
        logHeights(s"Rollback to height $height started:")
        while (bc.height > height) {
          bc.discardBlock()
        }
        if (height < persisted.height) {
          log.warn(s"Rollback to h=$height requested. Persisted height=${persisted.height}, will drop state and reapply blockchain now")
          persisted.clear()
          updatePersistedAndInMemory()
        } else {
          if (currentState.height != height) {
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

  override def processMicroBlock(microBlock: MicroBlock): Either[ValidationError, Unit] = ???
}

object BlockchainUpdaterImpl {

  def apply(
    persistedState: StateWriter with StateReader,
    history: HistoryWriter with History,
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

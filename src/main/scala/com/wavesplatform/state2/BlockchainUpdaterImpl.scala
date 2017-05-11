package com.wavesplatform.state2

import java.util.concurrent.locks.ReentrantReadWriteLock

import cats.kernel.Monoid
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.diffs.BlockDiffer
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.crypto.encode.Base58
import scorex.transaction._
import scorex.utils.ScorexLogging
import StateWriterImpl._
import BlockchainUpdaterImpl._

class BlockchainUpdaterImpl private(persisted: StateWriter with StateReader, settings: FunctionalitySettings,
                                    minimumInMemoryDiffSize: Int, bc: HistoryWriter with History,
                                    override val synchronizationToken: ReentrantReadWriteLock) extends BlockchainUpdater with ScorexLogging {

  private val MaxInMemDiff = minimumInMemoryDiffSize * 2

  private val unsafeDifferByRange: (StateReader, (Int, Int)) => BlockDiff = {
    case (sr, (from, to)) =>
      val blocks = measureLog(s"Reading blocks from $from up to $to") {
        Range(from, to).map(bc.blockAt(_).get)
      }
      measureLog(s"Building diff from $from up to $to") {
        BlockDiffer.unsafeDiffMany(settings)(sr, blocks)
      }
  }
  private val unsafeDiffAgainstPersistedByRange: ((Int, Int)) => BlockDiff = unsafeDifferByRange(persisted, _)

  var inMemoryDiff = Synchronized(Monoid[BlockDiff].empty)

  private def logHeights(prefix: String = ""): Unit = read { implicit l =>
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
    inMemoryDiff.swap(unsafeDiffAgainstPersistedByRange(persisted.height + 1, bc.height() + 1))
    logHeights("State rebuild finished:")
  }

  override def processBlock(block: Block): Either[ValidationError, Unit] = write { implicit l =>
    if (inMemoryDiff().heightDiff >= MaxInMemDiff) {
      updatePersistedAndInMemory()
    }
    for {
      blockDiff <- BlockDiffer(settings)(currentState, block)
      _ <- bc.appendBlock(block)
    } yield {
      log.info( s"""Block ${block.encodedId} appended. New height: ${bc.height()}, new score: ${bc.score()})""")
      inMemoryDiff.swap(Monoid[BlockDiff].combine(inMemoryDiff(), blockDiff))
    }
  }

  override def removeAfter(blockId: BlockId): Unit = write { implicit l =>
    bc.heightOf(blockId) match {
      case Some(height) =>
        while (bc.height > height) {
          bc.discardBlock()
        }
        if (height < persisted.height) {
          log.warn(s"Rollback to h=$height requested. Persisted height=${persisted.height}, will drop state and reapply blockchain now")
          persisted.clear()
          updatePersistedAndInMemory()

        } else {
          if (currentState.height != height) {
            inMemoryDiff.swap(unsafeDiffAgainstPersistedByRange(persisted.height + 1, height + 1))
          }
        }
      case None =>
        log.warn(s"removeAfter non-existing block ${Base58.encode(blockId)}")
    }
  }
}

object BlockchainUpdaterImpl {

  def apply(persisted: StateWriter with StateReader, settings: FunctionalitySettings, minimumInMemoryDiffSize: Int,
            bc: HistoryWriter with History, synchronizationToken: ReentrantReadWriteLock): Either[String, BlockchainUpdaterImpl] = {
    val blockchainUpdater = new BlockchainUpdaterImpl(persisted, settings, minimumInMemoryDiffSize, bc, synchronizationToken)
    if (persisted.height > bc.height())
      Left(s"storedBlocks = ${bc.height()}, statedBlocks=${persisted.height}")
    else {
      blockchainUpdater.logHeights("Start:")
      blockchainUpdater.updatePersistedAndInMemory()
      Right(blockchainUpdater)
    }
  }

  def ranges(from: Int, to: Int, by: Int): List[(Int, Int)] =
    if (from + by < to)
      (from, from + by) +: ranges(from + by, to, by)
    else List((from, to))

}

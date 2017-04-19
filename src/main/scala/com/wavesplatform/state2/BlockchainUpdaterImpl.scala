package com.wavesplatform.state2

import cats.kernel.Monoid
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.diffs.BlockDiffer
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import org.h2.mvstore.MVStore
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.crypto.encode.Base58
import scorex.transaction._
import scorex.utils.ScorexLogging

class BlockchainUpdaterImpl(persisted: StateWriter with StateReader, settings: FunctionalitySettings, bc: History)
  extends BlockchainUpdater with ScorexLogging {

  private val MinInMemDiff = 100
  private val MaxInMemDiff = 200

  private val unsafeDifferByRange: (StateReader, (Int, Int)) => BlockDiff = {
    case (sr, (from, to)) =>
      log.debug(s"Reading blocks from $from to $to")
      val blocks = Range(from, to).map(bc.blockAt(_).get)
      log.debug(s"Blocks read from $from to $to")
      val r = BlockDiffer.unsafeDiffMany(settings)(sr, blocks)
      log.debug(s"Diff for Range($from, $to) rebuilt")
      r
  }
  private val unsafeDiffByRange: ((Int, Int)) => BlockDiff = unsafeDifferByRange(persisted, _)
  private val unsafeDiffNotPersisted: () => BlockDiff = () => unsafeDiffByRange(persisted.height + 1, bc.height() + 1)

  @volatile var inMemoryDiff: BlockDiff = Monoid[BlockDiff].empty

  private def logHeights(prefix: String = ""): Unit =
    log.debug(s"$prefix Total blocks: ${bc.height()}, persisted: ${persisted.height}, imMemDiff: ${inMemoryDiff.heightDiff}")

  {
    logHeights("Start:")
    if (persisted.height > bc.height()) {
      throw new IllegalArgumentException(s"storedBlocks = ${bc.height()}, statedBlocks=${persisted.height}")
    } else {
      updatePersistedAndInMemory()
    }
  }

  def currentState: StateReader = CompositeStateReader.proxy(persisted, () => inMemoryDiff)

  private def updatePersistedAndInMemory(): Unit = {
    logHeights("States rebuild started:")
    val persistFrom = persisted.height + 1
    val persistUpTo = bc.height - MinInMemDiff + 1
    val diffToBePersisted = unsafeDiffByRange(persistFrom, persistUpTo)
    persisted.applyBlockDiff(diffToBePersisted)
    inMemoryDiff = unsafeDiffNotPersisted()
    logHeights("States rebuild finished:")
  }

  override def processBlock(block: Block): Either[ValidationError, Unit] = {
    if (inMemoryDiff.heightDiff >= MaxInMemDiff) {
      updatePersistedAndInMemory()
    }
    for {
      blockDiff <- BlockDiffer(settings)(currentState, block)
      _ <- bc.appendBlock(block)
    } yield {
      log.info( s"""Block ${block.encodedId} appended. New height: ${bc.height()}, new score: ${bc.score()})""")
      inMemoryDiff = Monoid[BlockDiff].combine(inMemoryDiff, blockDiff)
    }
  }

  override def removeAfter(blockId: BlockId): Unit = {
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
            inMemoryDiff = unsafeDiffByRange(persisted.height + 1, height + 1)
          }
        }
      case None =>
        log.warn(s"removeAfter non-existing block ${Base58.encode(blockId)}")
    }
  }
}

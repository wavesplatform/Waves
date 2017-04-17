package com.wavesplatform.state2

import cats.kernel.Monoid
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.diffs.BlockDiffer
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
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
      BlockDiffer.unsafeDiffMany(settings)(sr, Range(from, to).map(bc.blockAt(_).get))
  }
  private val unsafeDiffByRange: ((Int, Int)) => BlockDiff = unsafeDifferByRange(persisted, _)
  private val unsafeDiffNotPersisted: () => BlockDiff = () => unsafeDiffByRange(persisted.height + 1, bc.height() + 1)

  @volatile var inMemoryDiff: BlockDiff = {

    log.debug("Blockchain height: " + bc.height())
    log.debug("Persisted state height: " + persisted.height)

    if (persisted.height > bc.height()) {
      throw new IllegalArgumentException(s"storedBlocks = ${bc.height()}, statedBlocks=${persisted.height}")
    } else {
      log.debug("Rebuilding diff")
      val r = unsafeDiffNotPersisted()
      log.debug("Diff rebuilt successfully")
      r
    }
  }

  def currentState: StateReader = CompositeStateReader.proxy(persisted, () => inMemoryDiff)

  private def updateInMemoryDiffIfNeeded(): Unit = {
    if (inMemoryDiff.heightDiff >= MaxInMemDiff) {
      val diffToBePersisted = unsafeDiffByRange(persisted.height + 1, bc.height - MinInMemDiff + 1)
      persisted.applyBlockDiff(diffToBePersisted)
      inMemoryDiff = unsafeDiffNotPersisted()
      log.debug(s"Dumped blocks to persisted state. Last persisted block height: ${persisted.height}. In-memory height diff: ${inMemoryDiff.heightDiff}")
    }
  }

  override def processBlock(block: Block): Either[ValidationError, Unit] = {
    updateInMemoryDiffIfNeeded()
    for {
      blockDiff <- BlockDiffer(settings)(currentState, block)
      _ <- bc.appendBlock(block)
    } yield {
      log.info( s"""Block ${block.encodedId} appended. New height: ${bc.height()}, new score: ${bc.score()})""")
      inMemoryDiff = Monoid[BlockDiff].combine(inMemoryDiff, blockDiff)
    }
  }

  def rebuildState(): Unit = {
    persisted.clear()
    val toPersist = unsafeDiffByRange(1, bc.height - MinInMemDiff + 1)
    persisted.applyBlockDiff(toPersist)
    inMemoryDiff = unsafeDiffNotPersisted()
    log.info(s"State rebuilt. Persisted height=${persisted.height}")
  }

  override def removeAfter(blockId: BlockId): Unit = {
    bc.heightOf(blockId) match {
      case Some(height) =>
        while (bc.height > height) {
          bc.discardBlock()
        }
        if (height < persisted.height) {
          log.info(s"Rollback to h=$height requested. Persisted height=${persisted.height}, will drop state and reapply blockchain now")
          rebuildState()
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

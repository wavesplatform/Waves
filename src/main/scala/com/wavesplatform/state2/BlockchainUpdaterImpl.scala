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

  val unsafeDiffer: (StateReader, Seq[Block]) => BlockDiff = BlockDiffer.unsafeDiffMany(settings)

  private val MinInMemDiff = 100
  private val MaxInMemDiff = 200
  @volatile var inMemoryDiff: BlockDiff = {

    log.debug("Blockchain height: " + bc.height())
    log.debug("Persisted state height: " + persisted.height)

    if (persisted.height > bc.height()) {
      throw new IllegalArgumentException(s"storedBlocks = ${bc.height()}, statedBlocks=${persisted.height}")
    } else {
      log.debug("Resolving blocks to rebuild")
      val blocksToReconcile = Range(persisted.height + 1, bc.height() + 1)
        .map(h => bc.blockAt(h).get)
        .toList
      log.debug("Rebuilding diff")
      val r = unsafeDiffer(persisted, blocksToReconcile)
      log.debug("Diff rebuilt successfully")
      r
    }
  }

  def currentState: StateReader = CompositeStateReader.proxy(persisted, () => inMemoryDiff)

  private def updateInMemoryDiffIfNeeded(): Unit = {
    if (inMemoryDiff.heightDiff >= MaxInMemDiff) {
      val (persistBs, inMemBs) = Range(persisted.height + 1, persisted.height + inMemoryDiff.heightDiff + 1)
        .map(h => bc.blockAt(h).get)
        .toList
        .splitAt(inMemoryDiff.heightDiff - MinInMemDiff)
      val diffToBePersisted = unsafeDiffer(persisted, persistBs)
      persisted.applyBlockDiff(diffToBePersisted)
      inMemoryDiff = unsafeDiffer(persisted, inMemBs)
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
    val toPersist = ??? // Range(0, bc.height - MinInMemDiff)
    persisted.applyBlockDiff(toPersist)
    inMemoryDiff = ??? // Range(persisted.height + 1, bc.height() + 1) // probably reuse blocksToReconcile from var
  }

  override def removeAfter(blockId: BlockId): Unit = {
    bc.heightOf(blockId) match {
      case Some(height) =>
        while (bc.height > height) {
          bc.discardBlock()
        }
        if (height < persisted.height) {
          log.info(s"Rollback to h=$height requested. Persisted height=${persisted.height}, will drop state and reapply blockchain now")
          // dropping file is better
          persisted.clear()
          rebuildState()
          log.info(s"State rebuilt. Persisted height=${persisted.height}")
        } else {
          if (currentState.height != height) {
            inMemoryDiff = unsafeDiffer(persisted, Range(persisted.height + 1, height + 1).map(h => bc.blockAt(h).get))
          }
        }
      case None =>
        log.warn(s"RemoveAfter non-existing block ${Base58.encode(blockId)}")
    }
  }
}
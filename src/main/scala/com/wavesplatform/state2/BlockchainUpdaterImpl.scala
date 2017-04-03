package com.wavesplatform.state2

import cats.kernel.Monoid
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.diffs.BlockDiffer
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import scorex.block.Block
import scorex.transaction._
import scorex.utils.ScorexLogging

import scala.util.Try

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
      log.debug("Rebuilding diff")
      val blocksToReconcile = Range(persisted.height + 1, bc.height() + 1)
        .map(h => bc.blockAt(h).get)
        .toList
      val r = unsafeDiffer(persisted, blocksToReconcile)
      log.debug("Diff rebuilt successfully")
      r
    }
  }

  def composite: StateReader = new CompositeStateReader(persisted, inMemoryDiff)

  override def processBlock(block: Block): Try[Unit] = Try {
    val updatedInMemoryDiff =
      if (inMemoryDiff.heightDiff >= MaxInMemDiff) {
        val (persistBs, inMemBs) = Range(persisted.height + 1, persisted.height + inMemoryDiff.heightDiff + 1)
          .map(h => bc.blockAt(h).get)
          .toList
          .splitAt(MaxInMemDiff - MinInMemDiff)
        val diffToBePersisted = unsafeDiffer(persisted, persistBs)
        persisted.applyBlockDiff(diffToBePersisted)
        unsafeDiffer(persisted, inMemBs)
      } else {
        inMemoryDiff
      }

    BlockDiffer(settings)(composite, block) match {
      case Right(blockDiff) =>
        bc.appendBlock(block).map(_ =>
          inMemoryDiff = Monoid[BlockDiff].combine(updatedInMemoryDiff, blockDiff))
      case Left(m) =>
        throw new Exception(s"Block $block is not valid: $m")
    }
  }

  override def rollbackTo(height: Int): Unit = {
    if (height < persisted.height) {
      throw new IllegalArgumentException(s"cannot rollback to a block with height=$height, which is older than writer.height=${persisted.height}")
    } else {
      while (bc.height > height) {
        bc.discardBlock()
      }
      if (composite.height == height) {
      } else {
        inMemoryDiff = unsafeDiffer(persisted, Range(persisted.height + 1, height + 1).map(h => bc.blockAt(h).get))
      }
    }
  }
}
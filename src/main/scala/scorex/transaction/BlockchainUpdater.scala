package scorex.transaction

import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.utils.Synchronized

trait BlockchainUpdater extends Synchronized {
  def processBlock(block: Block): Either[ValidationError, Unit]

  def removeAfter(blockId: BlockId): Boolean
}


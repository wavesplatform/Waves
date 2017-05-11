package scorex.transaction

import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.utils.SynchronizedOver

trait BlockchainUpdater extends SynchronizedOver {
  def processBlock(block: Block): Either[ValidationError, Unit]

  def removeAfter(blockId: BlockId): Unit
}


package scorex.transaction

import scorex.block.Block
import scorex.block.Block.BlockId

trait BlockchainUpdater {
  def processBlock(block: Block): Either[ValidationError, Unit]

  def removeAfter(blockId: BlockId): Boolean
}


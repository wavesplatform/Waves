package scorex.transaction

import scorex.block.Block

trait BlockchainUpdater {
  def processBlock(block: Block): Either[ValidationError, Unit]

  def rollbackTo(height: Int): Unit
}


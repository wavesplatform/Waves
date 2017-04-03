package scorex.transaction

import scorex.block.Block

import scala.util.Try

trait BlockchainUpdater {
  def processBlock(block: Block): Try[Unit]

  def rollbackTo(height: Int): Unit
}


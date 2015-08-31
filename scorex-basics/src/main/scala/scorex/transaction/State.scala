package scorex.transaction

import scorex.block.Block

trait State {
  def processBlock(block: Block, reversal: Boolean): Unit
}

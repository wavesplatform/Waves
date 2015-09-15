package scorex.transaction

import scorex.block.Block

/**
 * Abstract functional interface of state which is a result of a sequential blocks applying
 */
trait State {
  def processBlock(block: Block, reversal: Boolean): Unit
}

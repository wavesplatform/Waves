package scorex.transaction

import scorex.block.Block

/**
  * Abstract functional interface of state which is a result of a sequential blocks applying
  */
trait State {
  private[transaction] def processBlock(block: Block, reversal: Boolean): Unit

  private[transaction] def processBlock(block: Block): Unit = processBlock(block, reversal = false)
}

package scorex.transaction

import scorex.block.Block

import scala.util.Try

/**
  * Abstract functional interface of state which is a result of a sequential blocks applying
  */
trait State {
  private[transaction] def processBlock(block: Block, reversal: Boolean): Try[State]

  private[transaction] def processBlock(block: Block): Try[State] = processBlock(block, reversal = false)
}

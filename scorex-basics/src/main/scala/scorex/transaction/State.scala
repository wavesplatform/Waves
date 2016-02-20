package scorex.transaction

import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.transaction.BlockStorage.{Direction, Forward, Reversed}

import scala.util.Try

/**
  * Abstract functional interface of state which is a result of a sequential blocks applying
  */
trait State {
  /**
    * Apply block to current state or revert it, if reversal=true.
    */
  private[transaction] def processBlock(block: Block): Try[State]

  def isValid(tx: Transaction): Boolean = isValid(Seq(tx))

  def isValid(txs: Seq[Transaction], height: Option[Int] = None): Boolean = validate(txs, height).size == txs.size

  def validate(txs: Seq[Transaction], height: Option[Int] = None): Seq[Transaction]

  def rollbackTo(height: Int): Unit
}

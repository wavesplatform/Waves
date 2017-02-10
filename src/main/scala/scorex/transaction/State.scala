package scorex.transaction

import scorex.block.Block
import scorex.transaction.ValidationError.StateValidationError

import scala.util.Try

/**
  * Abstract functional interface of state which is a result of a sequential blocks applying
  */
trait State {
  private[transaction] def processBlock(block: Block): Try[State]

  def isValid(tx: Transaction, blockTime: Long): Boolean = isValid(Seq(tx), blockTime = blockTime)

  def isValid(txs: Seq[Transaction], height: Option[Int] = None, blockTime: Long): Boolean = validate(txs, height, blockTime).size == txs.size

  def validate(txs: Seq[Transaction], height: Option[Int] = None, blockTime: Long): Seq[Transaction]

  def included(signature: Array[Byte]): Option[Int]

  private[transaction] def rollbackTo(height: Int): State
}

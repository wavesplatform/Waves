package scorex.transaction

import scorex.block.Block

import scala.util.Try

trait State {
  def allValid(txs: Seq[Transaction], blockTime: Long): Boolean = validate(txs, blockTime).size == txs.size

  def validate(txs: Seq[Transaction], blockTime: Long): Seq[Transaction]

  def included(signature: Array[Byte]): Option[Int]

  private[transaction] def processBlock(block: Block): Try[State]

  private[transaction] def rollbackTo(height: Int): State
}

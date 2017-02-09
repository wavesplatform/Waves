package scorex.transaction

import scorex.block.Block
import scala.util.Try
import scala.language.implicitConversions

trait State {

  def validate(txs: Seq[Transaction], blockTime: Long): Seq[Transaction] // Seq[Either[ValidationError,Transaction]]

  def included(signature: Array[Byte]): Option[Int]

  private[transaction] def processBlock(block: Block): Try[State]

  private[transaction] def rollbackTo(height: Int): State
}

object State {
  implicit def richState(s: State): RichState = new RichState(s)

  class RichState(s: State) {

    def validateOne(trans: Transaction, blockTime: Long): Option[Transaction] = s.validate(Seq(trans), blockTime).headOption

    def allValid(txs: Seq[Transaction], blockTime: Long): Boolean = s.validate(txs, blockTime).size == txs.size

    def isValid(tx: Transaction, blockTime: Long): Boolean = s.validateOne(tx, blockTime).isDefined
  }
}

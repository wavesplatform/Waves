package scorex.transaction

import scorex.account.Account
import scorex.block.Block

import scala.util.Try

trait State {
  private[transaction] def processBlock(block: Block): Try[State]

  def isValid(tx: Transaction, blockTime: Long): Boolean = isValid(Seq(tx), blockTime = blockTime)

  def isValid(txs: Seq[Transaction], height: Option[Int] = None, blockTime: Long): Boolean = validate(txs, height, blockTime).size == txs.size

  def validate(txs: Seq[Transaction], height: Option[Int] = None, blockTime: Long): Seq[Transaction]

  def included(signature: Array[Byte]): Option[Int]

  private[transaction] def rollbackTo(height: Int): State

  def balance(account: Account, height: Option[Int] = None): Long

  def balanceWithConfirmations(account: Account, confirmations: Int, heightOpt: Option[Int] = None): Long

  def accountTransactions(account: Account, limit: Int): Seq[_ <: Transaction]
}

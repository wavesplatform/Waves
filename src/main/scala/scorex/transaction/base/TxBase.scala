package scorex.transaction.base

import scorex.account.PublicKeyAccount
import scorex.transaction.Transaction

trait TxBase extends Transaction {
  def sender: PublicKeyAccount
  def fee: Long
  def timestamp: Long
}

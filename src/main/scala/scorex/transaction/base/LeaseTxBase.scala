package scorex.transaction.base

import scorex.account.AddressOrAlias

trait LeaseTxBase extends TxBase {
  def amount: Long
  def recipient: AddressOrAlias
}

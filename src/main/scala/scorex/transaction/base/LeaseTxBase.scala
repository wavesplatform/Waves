package scorex.transaction.base

import scorex.account.AddressOrAlias
import scorex.transaction.Transaction

trait LeaseTxBase { _: Transaction =>
  def amount: Long
  def recipient: AddressOrAlias
}

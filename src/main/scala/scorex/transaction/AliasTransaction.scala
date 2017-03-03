package scorex.transaction

import scorex.account.Alias


sealed trait AliasTransaction extends SignedTransaction {
  def alias: Alias
  def fee : Long
}

package scorex.transaction.base

import scorex.account.Alias

trait CreateAliasTxBase extends TxBase {
  def alias: Alias
}

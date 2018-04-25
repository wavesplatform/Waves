package scorex.transaction.base

import scorex.account.Alias
import scorex.transaction.Transaction

trait CreateAliasTxBase { _: Transaction =>
  def alias: Alias
}

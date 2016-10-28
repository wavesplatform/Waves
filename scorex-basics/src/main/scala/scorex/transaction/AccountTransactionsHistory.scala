package scorex.transaction

import scorex.account.Account

trait AccountTransactionsHistory {
  def accountTransactions(account: Account, limit: Int = 50): Array[_ <: Transaction]
}

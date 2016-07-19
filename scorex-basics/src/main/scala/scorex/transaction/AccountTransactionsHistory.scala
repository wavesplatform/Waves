package scorex.transaction

import scorex.account.Account

trait AccountTransactionsHistory {
  def accountTransactions(account: Account): Array[_ <: Transaction]
}

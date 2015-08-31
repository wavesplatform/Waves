package scorex.transaction

import scorex.account.Account

trait AccountTransactionsHistory {
  def accountTransactions(address: String): Seq[Transaction] = {
    Account.isValidAddress(address) match {
      case false => Seq()
      case true =>
        val acc = new Account(address)
        accountTransactions(acc)
    }
  }

  def accountTransactions(account: Account): Seq[Transaction]

  def watchAccountTransactions(account: Account)

  def stopWatchingAccountTransactions(account: Account)
}

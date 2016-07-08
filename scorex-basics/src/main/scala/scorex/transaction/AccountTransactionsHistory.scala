package scorex.transaction

import scorex.account.Account

trait AccountTransactionsHistory {
  def accountTransactions(address: String): Array[_ <: Transaction] = {
    Account.isValidAddress(address) match {
      case false => Array()
      case true => accountTransactions(new Account(address)).distinct
    }
  }

  def accountTransactions(account: Account): Array[_ <: Transaction]

}

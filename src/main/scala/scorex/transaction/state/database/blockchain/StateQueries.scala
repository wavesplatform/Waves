package scorex.transaction.state.database.blockchain

import scorex.account.Account
import scorex.crypto.SigningFunctionsImpl
import scorex.transaction.Transaction


trait StateQueries {
  def balance(address: String, confirmations: Int): BigDecimal

  def balance(address: String): BigDecimal = balance(address, 0)

  def accountTransactions(address: String): Seq[Transaction] = {
    Account.isValidAddress(address) match {
      case false => Seq()
      case true =>
        val acc = new Account(address)
        accountTransactions(acc)
    }
  }

  def watchAccountTransactions(account: Account)

  def stopWatchingAccountTransactions(account: Account)

  def accountTransactions(account: Account): Seq[Transaction]

  def generationBalance(address: String): BigDecimal = balance(address, 50)

  def generationBalance(account: Account): BigDecimal = balance(account.address, 50)
}

package database

import scorex.account.Account
import scorex.crypto.Crypto
import scorex.transaction.Transaction


trait StateQuery {
  def balance(address: String, fromHeight: Int, confirmations: Int): BigDecimal

  def balance(address: String): BigDecimal = balance(address, 0, 0)

  def accountTransactions(account: String): Seq[Transaction] = {
    Crypto.isValidAddress(account) match {
      case false => Seq()
      case true =>
        val acc = new Account(account)
        accountTransactions(acc)
    }
  }

  def accountTransactions(account: Account): Seq[Transaction]

  def generationBalance(address: String): BigDecimal = balance(address, 0, 50)
}

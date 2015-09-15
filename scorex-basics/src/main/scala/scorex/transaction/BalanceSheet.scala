package scorex.transaction

import scorex.account.Account


trait BalanceSheet {
  def balance(address: String, confirmations: Int): Long

  def balance(address: String): Long = balance(address, 0)

  def generationBalance(address: String): Long = balance(address, 50)

  def generationBalance(account: Account): Long = balance(account.address, 50)
}

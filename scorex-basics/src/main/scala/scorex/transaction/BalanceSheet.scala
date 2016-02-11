package scorex.transaction

import scorex.account.Account


trait BalanceSheet {
  def balance(address: String, height: Option[Int] = None): Long

  def balance(address: String, confirmations: Int): Long

  def generationBalance(address: String): Long = balance(address, 50)

  def generationBalance(account: Account): Long = generationBalance(account.address)
}

package scorex.transaction

import scorex.account.Account

trait BalanceSheet {
  def balance(account: Account, height: Option[Int] = None): Long
  def effectiveBalance(account: Account, height: Option[Int] = None): Long

  def balanceWithConfirmations(account: Account, confirmations: Int, heightOpt: Option[Int] = None): Long
  def effectiveBalanceWithConfirmations(account: Account, confirmations: Int, heightOpt: Option[Int] = None): Long
}

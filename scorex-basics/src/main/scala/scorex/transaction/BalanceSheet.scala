package scorex.transaction

import scorex.account.Account


trait BalanceSheet {
  def balance(account: Account, height: Option[Int] = None): Long

  /**
   *
   * @return Minimum balance from current block to balance confirmation blocks ago
   */
  def balanceWithConfirmations(account: Account, confirmations: Int): Long
}

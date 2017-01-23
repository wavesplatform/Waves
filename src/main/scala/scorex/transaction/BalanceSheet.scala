package scorex.transaction

import scorex.account.Account


trait BalanceSheet {
  def balance(account: Account, height: Option[Int] = None): Long

  /**
   *
   * @return Minimum balance from block at height "<code>heightOpt</code>" to "<code>confirmation</code>" blocks ago
   */
  def balanceWithConfirmations(account: Account, confirmations: Int, heightOpt: Option[Int] = None): Long
}

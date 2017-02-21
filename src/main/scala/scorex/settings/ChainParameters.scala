package scorex.settings

import scorex.account.AddressScheme
import scorex.transaction.Transaction

trait ChainParameters {
  def allowTemporaryNegativeUntil: Long
  def allowInvalidPaymentTransactionsByTimestamp: Long
  def requireSortedTransactionsAfter: Long
  def generatingBalanceDepthFrom50To1000AfterHeight: Long
  def minimalGeneratingBalanceAfterTimestamp: Long
  def allowTransactionsFromFutureUntil: Long
  def allowUnissuedAssetsUntil: Long
  def allowBurnTransactionAfterTimestamp: Long
  def allowLeaseTransactionAfterTimestamp: Long
  def requirePaymentUniqueId: Long

  def initialBalance: Long
  def genesisTimestamp: Long
  def genesisTxs: Seq[Transaction]
  def addressScheme: AddressScheme
}

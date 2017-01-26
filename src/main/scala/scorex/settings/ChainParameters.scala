package scorex.settings

import scorex.account.AddressScheme
import scorex.transaction.Transaction

object ChainParameters {
  val Disabled = new AnyRef with ChainParameters {
    override def allowTemporaryNegativeUntil: Long = Long.MaxValue
    override def requireSortedTransactionsAfter: Long = Long.MaxValue
    override def allowInvalidPaymentTransactionsByTimestamp: Long = Long.MaxValue
    override def generatingBalanceDepthFrom50To1000AfterHeight: Long = Long.MaxValue
    override def minimalGeneratingBalanceAfterTimestamp: Long = Long.MaxValue
    override def allowTransactionsFromFutureUntil: Long = Long.MaxValue
    override def allowUnissuedAssetsUntil: Long = Long.MaxValue
    override def allowBurnTransactionAfterTimestamp: Long = Long.MaxValue
    override def requirePaymentUniqueId: Long = Long.MaxValue

    override def initialBalance: Long = 100000000000000L

    override def genesisTimestamp: Long = ???

    override def genesisTxs: Seq[Transaction] = ???

    override def addressScheme: AddressScheme = ???
  }

  val Enabled = new AnyRef with ChainParameters {
    override def allowTemporaryNegativeUntil: Long = 0
    override def requireSortedTransactionsAfter: Long = 0
    override def allowInvalidPaymentTransactionsByTimestamp: Long = 0
    override def generatingBalanceDepthFrom50To1000AfterHeight: Long = 0
    override def minimalGeneratingBalanceAfterTimestamp: Long = 0
    override def allowTransactionsFromFutureUntil: Long = 0
    override def allowUnissuedAssetsUntil: Long = 0
    override def allowBurnTransactionAfterTimestamp: Long = 0
    override def requirePaymentUniqueId: Long = 0

    override def initialBalance: Long = ???

    override def genesisTimestamp: Long = ???

    override def genesisTxs: Seq[Transaction] = ???

    override def addressScheme: AddressScheme = ???
  }
}
trait ChainParameters {
  def allowTemporaryNegativeUntil: Long
  def allowInvalidPaymentTransactionsByTimestamp: Long
  def requireSortedTransactionsAfter: Long
  def generatingBalanceDepthFrom50To1000AfterHeight: Long
  def minimalGeneratingBalanceAfterTimestamp: Long
  def allowTransactionsFromFutureUntil: Long
  def allowUnissuedAssetsUntil: Long
  def allowBurnTransactionAfterTimestamp: Long
  def requirePaymentUniqueId: Long
  def initialBalance: Long
  def genesisTimestamp: Long
  def genesisTxs: Seq[Transaction]
  def addressScheme: AddressScheme
}

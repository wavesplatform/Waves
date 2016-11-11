package scorex.settings

object WavesHardForkParameters {
  val Disabled = new AnyRef with WavesHardForkParameters {
    override def allowTemporaryNegativeUntil: Long = Long.MaxValue
    override def requireSortedTransactionsAfter: Long = Long.MaxValue
    override def allowInvalidPaymentTransactionsByTimestamp: Long = Long.MaxValue
  }

  val Enabled = new AnyRef with WavesHardForkParameters {
    override def allowTemporaryNegativeUntil: Long = 0
    override def requireSortedTransactionsAfter: Long = 0
    override def allowInvalidPaymentTransactionsByTimestamp: Long = 0
  }
}
trait WavesHardForkParameters {
  def allowTemporaryNegativeUntil: Long
  def allowInvalidPaymentTransactionsByTimestamp: Long
  def requireSortedTransactionsAfter: Long
}

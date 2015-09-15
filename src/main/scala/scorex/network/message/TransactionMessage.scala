package scorex.network.message

import scorex.transaction.LagonakiTransaction

case class TransactionMessage(transaction: LagonakiTransaction) extends Message {

  override val messageType = Message.TransactionType

  override lazy val dataBytes = transaction.bytes()
}

object TransactionMessage {
  def apply(data: Array[Byte]): TransactionMessage = new TransactionMessage(LagonakiTransaction.parse(data))
}
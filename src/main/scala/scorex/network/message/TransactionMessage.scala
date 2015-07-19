package scorex.network.message

import scorex.transaction.Transaction

case class TransactionMessage(transaction: Transaction) extends Message {

  override val messageType = Message.TransactionType

  override lazy val dataBytes = transaction.bytes()
}

object TransactionMessage {
  def apply(data: Array[Byte]): TransactionMessage = new TransactionMessage(Transaction.parse(data))
}
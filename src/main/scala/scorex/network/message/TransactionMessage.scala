package scorex.network.message

import com.google.common.primitives.Bytes
import scorex.transaction.Transaction

case class TransactionMessage(transaction: Transaction) extends Message {

  override val messageType = Message.TRANSACTION_TYPE

  override def toBytes() = {
    val data = transaction.toBytes()
    Bytes.concat(super.toBytes(), generateChecksum(data), data)
  }

  override protected def getDataLength() = transaction.dataLength
}

object TransactionMessage {
  def apply(data: Array[Byte]): TransactionMessage = new TransactionMessage(Transaction.fromBytes(data))
}
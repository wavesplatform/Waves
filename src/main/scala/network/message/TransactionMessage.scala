package network.message

import network.ConnectedPeer
import scorex.transaction.Transaction
import scorex.transaction.TransactionFactory
import com.google.common.primitives.Bytes

case class TransactionMessage(transaction: Transaction, mbSender: Option[ConnectedPeer] = None, mbId: Option[Int] = None) extends Message {

  override val messageType = Message.TRANSACTION_TYPE

  override def toBytes() = {
    val data = transaction.toBytes
    Bytes.concat(super.toBytes(), generateChecksum(data), data)
  }

  override protected def getDataLength() = transaction.dataLength
}


object TransactionMessage {
  def apply(data: Array[Byte]): TransactionMessage = {
    //PARSE TRANSACTION
    val transaction = TransactionFactory.parse(data)
    new TransactionMessage(transaction)
  }
}
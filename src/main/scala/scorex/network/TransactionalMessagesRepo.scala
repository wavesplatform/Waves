package scorex.network

import scorex.network.message.Message.MessageCode
import scorex.network.message.MessageSpec
import scorex.transaction.{Transaction, TypedTransaction}

import scala.util.Try

object TransactionalMessagesRepo {

  object TransactionMessageSpec extends MessageSpec[Transaction] {
    override val messageCode: MessageCode = 25: Byte

    override val messageName: String = "Transaction message"

    override def deserializeData(bytes: Array[MessageCode]): Try[Transaction] =
      TypedTransaction.parseBytes(bytes)

    override def serializeData(tx: Transaction): Array[MessageCode] = tx.bytes
  }

  val specs = Seq(TransactionMessageSpec)
}

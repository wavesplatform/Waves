package com.wavesplatform.it.network.client

import scorex.network.message.Message.MessageCode
import scorex.network.message.MessageSpec
import scorex.transaction.{Transaction, TransactionParser}

import scala.util.Try

object TransactionalMessagesRepo {

  object TransactionMessageSpec extends MessageSpec[Transaction] {
    override val messageCode: MessageCode = 25: Byte

    override val messageName: String = "Transaction message"

    override def deserializeData(bytes: Array[MessageCode]): Try[Transaction] =
      TransactionParser.parseBytes(bytes)

    override def serializeData(tx: Transaction): Array[MessageCode] = tx.bytes
  }

  val specs = Seq(TransactionMessageSpec)
}

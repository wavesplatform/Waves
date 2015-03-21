package scorex.database

import scorex.transaction.Transaction

import scala.collection.mutable


object UnconfirmedTransactionsDatabaseImpl extends UnconfirmedTransactionsDatabase {
  val transactions = mutable.Buffer[Transaction]()

  override def put(tx: Transaction): Boolean = {
    transactions += tx
    true
  }

  override def remove(tx: Transaction): Unit = transactions -= tx

  override def getAll(): Seq[Transaction] = transactions.toSeq

  override def getBySignature(signature: Array[Byte]): Option[Transaction] = transactions.find(_.signature == signature)
}

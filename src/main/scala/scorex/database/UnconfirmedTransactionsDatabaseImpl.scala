package scorex.database

import scorex.transaction.Transaction
import scala.collection.concurrent.TrieMap


object UnconfirmedTransactionsDatabaseImpl extends UnconfirmedTransactionsDatabase {
  val transactions = TrieMap[Array[Byte], Transaction]()

  override def put(tx: Transaction): Boolean = {
    transactions += tx.signature -> tx
    true
  }

  override def remove(tx: Transaction): Unit = transactions -= tx.signature

  override def getAll(): Seq[Transaction] = transactions.values.toSeq

  override def getBySignature(signature: Array[Byte]): Option[Transaction] = transactions.get(signature)
}

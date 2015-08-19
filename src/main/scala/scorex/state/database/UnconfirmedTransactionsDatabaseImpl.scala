package scorex.state.database

import com.google.common.primitives.Longs
import scorex.transaction.Transaction

import scala.collection.concurrent.TrieMap


object UnconfirmedTransactionsDatabaseImpl extends UnconfirmedTransactionsDatabase {
  val transactions = TrieMap[Long, Transaction]()

  private def key(tx:Transaction):Long = key(tx.signature)

  private def key(signature: Array[Byte]):Long = {
    Longs.fromByteArray(signature.take(8))
  }

  override def putIfNew(tx: Transaction): Boolean =
    transactions.putIfAbsent(key(tx), tx).isEmpty

  override def remove(tx: Transaction): Unit = transactions -= key(tx)

  override def all(): Seq[Transaction] = transactions.values.toSeq

  override def getBySignature(signature: Array[Byte]): Option[Transaction] = transactions.get(key(signature))
}

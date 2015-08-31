package scorex.transaction.state.database

import com.google.common.primitives.Longs
import scorex.transaction.LagonakiTransaction

import scala.collection.concurrent.TrieMap


object UnconfirmedTransactionsDatabaseImpl extends UnconfirmedTransactionsDatabase {
  val transactions = TrieMap[Long, LagonakiTransaction]()

  private def key(tx: LagonakiTransaction): Long = key(tx.signature)

  private def key(signature: Array[Byte]): Long = {
    Longs.fromByteArray(signature.take(8))
  }

  override def putIfNew(tx: LagonakiTransaction): Boolean =
    transactions.putIfAbsent(key(tx), tx).isEmpty

  override def remove(tx: LagonakiTransaction): Unit = transactions -= key(tx)

  override def all(): Seq[LagonakiTransaction] = transactions.values.toSeq

  override def getBySignature(signature: Array[Byte]): Option[LagonakiTransaction] = transactions.get(key(signature))
}

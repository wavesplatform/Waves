package scorex.transaction.state.database

import com.google.common.primitives.Longs
import scorex.transaction.LagonakiTransaction

import scala.collection.concurrent.TrieMap


object UnconfirmedTransactionsDatabaseImpl extends UnconfirmedTransactionsDatabase {
  val transactions = TrieMap[Long, LagonakiTransaction]()

  //using Long instead of Array[Byte] just for performance improvement
  private def key(signature: Array[Byte]): Long = {
    Longs.fromByteArray(signature.take(8))
  }

  private def key(tx: LagonakiTransaction): Long = key(tx.signature)

  override def putIfNew(tx: LagonakiTransaction): Boolean =
    transactions.putIfAbsent(key(tx), tx).isEmpty

  override def remove(tx: LagonakiTransaction): Unit = transactions -= key(tx)

  override def all(): Seq[LagonakiTransaction] = transactions.values.toSeq

  override def getBySignature(signature: Array[Byte]): Option[LagonakiTransaction] = transactions.get(key(signature))
}

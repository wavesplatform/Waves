package scorex.transaction.state.database

import com.google.common.primitives.Longs
import scorex.transaction.Transaction
import scorex.utils.ScorexLogging

import scala.collection.concurrent.TrieMap


object UnconfirmedTransactionsDatabaseImpl extends UnconfirmedTransactionsDatabase with ScorexLogging {

  //TODO move to config
  val SizeLimit = 1000

  val transactions = TrieMap[Long, Transaction]()

  //using Long instead of Array[Byte] just for performance improvement
  private def key(signature: Array[Byte]): Long =
    Longs.fromByteArray(signature.take(8))

  private def key(tx: Transaction): Long = key(tx.signature)

  override def putIfNew(tx: Transaction): Boolean = if (transactions.size < SizeLimit) {
    transactions.putIfAbsent(key(tx), tx).isEmpty
  } else {
    log.warn("Transaction pool size limit is reached")
    false
  }

  override def remove(tx: Transaction): Unit = transactions -= key(tx)

  override def all(): Seq[Transaction] = transactions.values.toSeq

  override def getBySignature(signature: Array[Byte]): Option[Transaction] = transactions.get(key(signature))
}

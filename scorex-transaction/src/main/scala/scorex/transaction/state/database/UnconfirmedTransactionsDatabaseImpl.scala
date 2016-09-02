package scorex.transaction.state.database

import scorex.transaction.{Transaction, UnconfirmedTransactionsStorage}
import scorex.utils.ScorexLogging

import scala.collection.concurrent.TrieMap


class UnconfirmedTransactionsDatabaseImpl(val sizeLimit: Int = 1000) extends UnconfirmedTransactionsStorage with ScorexLogging {

  private type TxKey = scorex.network.BlockchainSynchronizer.InnerId

  private val transactions = TrieMap[TxKey, Transaction]()

  private def key(signature: Array[Byte]): TxKey = new TxKey(signature)

  private def key(tx: Transaction): TxKey = key(tx.signature)

  override def putIfNew(tx: Transaction, txValidator: Transaction => Boolean): Boolean =
    if (transactions.size < sizeLimit) {
      if (txValidator(tx)) {
        transactions.putIfAbsent(key(tx), tx).isEmpty
      } else {
        log.error(s"Transaction $tx is not valid")
        false
      }
    } else {
      log.warn("Transaction pool size limit is reached")
      false
    }

  override def remove(tx: Transaction): Unit = transactions -= key(tx)

  override def all(): Seq[Transaction] = transactions.values.toSeq

  override def getBySignature(signature: Array[Byte]): Option[Transaction] = transactions.get(key(signature))
}

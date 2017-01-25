package scorex.transaction.state.database

import scorex.settings.Settings
import scorex.transaction.{Transaction, UnconfirmedTransactionsStorage}
import scorex.utils.ScorexLogging

import scala.collection.concurrent.TrieMap
import scala.collection.mutable


class UnconfirmedTransactionsDatabaseImpl(settings: Settings) extends UnconfirmedTransactionsStorage with ScorexLogging {

  private type TxKey = mutable.WrappedArray.ofByte

  private val transactions = TrieMap[TxKey, Transaction]()

  private def key(id: Array[Byte]): TxKey = new TxKey(id)

  private def key(tx: Transaction): TxKey = key(tx.id)

  override def putIfNew(tx: Transaction, txValidator: Transaction => Boolean): Boolean =
    if (transactions.size < settings.utxSize) {
      val txKey = key(tx)
      if (transactions.contains(txKey)) {
        false
      } else if (txValidator(tx)) {
        transactions.update(txKey, tx)
        true
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

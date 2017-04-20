package scorex.transaction.state.database

import com.wavesplatform.settings.UTXSettings
import scorex.transaction.ValidationError.{TransactionParameterValidationError, TransactionValidationError}
import scorex.transaction.{Transaction, UnconfirmedTransactionsStorage, ValidationError}
import scorex.utils.ScorexLogging

import scala.collection.concurrent.TrieMap
import scala.collection.mutable


class UnconfirmedTransactionsDatabaseImpl(settings: UTXSettings) extends UnconfirmedTransactionsStorage with ScorexLogging {

  private type TxKey = mutable.WrappedArray.ofByte

  private val transactions = TrieMap[TxKey, Transaction]()

  private def key(id: Array[Byte]): TxKey = new TxKey(id)

  private def key(tx: Transaction): TxKey = key(tx.id)

    if (transactions.size < settings.size) {
      val txKey = key(tx)
      if (transactions.contains(txKey)) {
        Left(TransactionValidationError(tx, "already in the pool"))
      } else txValidator(tx) match {
        case Right(t) =>
          transactions.update(txKey, tx)
          Right(t)
        case Left(err) =>
          log.debug(err.toString)
          Left(err)
      }
    } else {
      Left(TransactionValidationError(tx, "Transaction pool size limit is reached"))
    }

  override def remove(tx: Transaction): Unit = transactions -= key(tx)

  override def all(): Seq[Transaction] = transactions.values.toSeq

  override def getBySignature(signature: Array[Byte]): Option[Transaction] = transactions.get(key(signature))
}

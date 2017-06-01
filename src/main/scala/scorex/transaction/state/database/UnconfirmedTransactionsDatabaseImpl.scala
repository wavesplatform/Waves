package scorex.transaction.state.database

import com.wavesplatform.settings.UTXSettings
import com.wavesplatform.state2.{ByteArray, EqByteArray}
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction.{Transaction, UnconfirmedTransactionsStorage, ValidationError}
import scorex.utils.ScorexLogging

import scala.collection.concurrent.TrieMap


class UnconfirmedTransactionsDatabaseImpl(size: Int) extends UnconfirmedTransactionsStorage with ScorexLogging {

  private val transactions = TrieMap[ByteArray, Transaction]()

  override def putIfNew[T <: Transaction](tx: T, txValidator: T => Either[ValidationError, T]): Either[ValidationError, T] =
    if (transactions.size < size) {
      if (transactions.contains(tx.id)) {
        Left(TransactionValidationError(tx, "already in the pool"))
      } else txValidator(tx) match {
        case Right(t) =>
          transactions.update(tx.id, tx)
          Right(t)
        case Left(err) =>
          log.debug(err.toString)
          Left(err)
      }
    } else {
      Left(TransactionValidationError(tx, "Transaction pool size limit is reached"))
    }

  override def remove(tx: Transaction): Unit = transactions -= tx.id

  override def all(): Seq[Transaction] = transactions.values.toSeq

  override def getBySignature(signature: ByteArray): Option[Transaction] = transactions.get(signature)
}

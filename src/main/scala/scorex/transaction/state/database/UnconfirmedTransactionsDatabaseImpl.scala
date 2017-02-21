package scorex.transaction.state.database

import com.wavesplatform.settings.UTXSettings
import scorex.transaction.ValidationError.CustomValidationError
import scorex.transaction.{Transaction, UnconfirmedTransactionsStorage, ValidationError}
import scorex.utils.ScorexLogging

import scala.collection.concurrent.TrieMap
import scala.collection.mutable


class UnconfirmedTransactionsDatabaseImpl(settings: UTXSettings) extends UnconfirmedTransactionsStorage with ScorexLogging {

  private type TxKey = mutable.WrappedArray.ofByte

  private val transactions = TrieMap[TxKey, Transaction]()

  private def key(id: Array[Byte]): TxKey = new TxKey(id)

  private def key(tx: Transaction): TxKey = key(tx.id)

  override def putIfNew[T <: Transaction](tx: T, txValidator: T => Either[ValidationError, T]): Either[ValidationError, T] =
    if (transactions.size < settings.size) {
      val txKey = key(tx)
      if (transactions.contains(txKey)) {
        Left(CustomValidationError(s"Transaction $tx already in the pool"))
      } else txValidator(tx) match {
        case Right(t) =>
          transactions.update(txKey, tx)
          Right(t)
        case Left(err) =>
          log.error(s"Transaction $tx is not valid")
          Left(err)
      }
    } else {
      Left(CustomValidationError("Transaction pool size limit is reached"))
    }

  override def remove(tx: Transaction): Unit = transactions -= key(tx)

  override def all(): Seq[Transaction] = transactions.values.toSeq

  override def getBySignature(signature: Array[Byte]): Option[Transaction] = transactions.get(key(signature))
}

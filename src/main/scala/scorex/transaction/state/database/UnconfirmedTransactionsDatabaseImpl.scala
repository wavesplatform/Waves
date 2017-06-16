package scorex.transaction.state.database

import com.wavesplatform.settings.UTXSettings
import com.wavesplatform.state2.ByteStr
import scorex.transaction.ValidationError.{GenericError}
import scorex.transaction.{Transaction, UnconfirmedTransactionsStorage, ValidationError}

import scala.collection.concurrent.TrieMap


class UnconfirmedTransactionsDatabaseImpl(size: Int) extends UnconfirmedTransactionsStorage {

  private val transactions = TrieMap[ByteStr, Transaction]()

  override def putIfNew[T <: Transaction](tx: T, txValidator: T => Either[ValidationError, T]): Either[ValidationError, T] =
    if (transactions.size < size) {
      if (transactions.contains(tx.id)) {
        Left(GenericError("already in the pool"))
      } else {
        val result = txValidator(tx)
        result.foreach(t => transactions.update(tx.id, t))
        result
      }
    } else {
      Left(GenericError("Transaction pool size limit is reached"))
    }

  override def remove(tx: Transaction): Unit = transactions -= tx.id

  override def all(): Seq[Transaction] = transactions.values.toSeq

  override def getBySignature(signature: ByteStr): Option[Transaction] = transactions.get(signature)
}

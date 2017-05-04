package scorex.transaction

import com.wavesplatform.settings.{BlockchainSettings, FunctionalitySettings}
import com.wavesplatform.state2.Validator
import com.wavesplatform.state2.reader.StateReader
import scorex.block.Block
import scorex.consensus.TransactionsOrdering
import scorex.utils.Time

import scala.concurrent.duration._

trait UnconfirmedTransactionsStorage {
  def putIfNew[T <: Transaction](tx: T, txValidator: T => Either[ValidationError, T]): Either[ValidationError, T]

  def all(): Seq[Transaction]

  def getBySignature(signature: Array[Byte]): Option[Transaction]

  def remove(tx: Transaction)
}

object UnconfirmedTransactionsStorage {

  val MaxTimeUtxFuture: FiniteDuration = 15.seconds
  val MaxTimeUtxPast: FiniteDuration = 90.minutes

  def clearIncorrectTransactions(fs: FunctionalitySettings, stateReader: StateReader, utx: UnconfirmedTransactionsStorage, time: Time): Unit = {
    val currentTime = time.correctedTime()
    val txs = utx.all()
    val notExpired = txs.filter { tx => (currentTime - tx.timestamp).millis <= MaxTimeUtxPast }
    val notFromFuture = notExpired.filter { tx => (tx.timestamp - currentTime).millis <= MaxTimeUtxFuture }
    val inOrder = notFromFuture.sorted(TransactionsOrdering.InUTXPool)
    val valid = Validator.validate(fs, stateReader, inOrder, None, currentTime)._2
    txs.diff(valid).foreach(utx.remove)
  }

  def packUnconfirmed(history: History, state: StateReader, fs: FunctionalitySettings, utx: UnconfirmedTransactionsStorage, time: Time)
                     (heightOpt: Option[Int]): Seq[Transaction] = {
    clearIncorrectTransactions(fs, state, utx, time)
    val txs = utx.all()
      .sorted(TransactionsOrdering.InUTXPool)
      .take(Block.MaxTransactionsPerBlock)
      .sorted(TransactionsOrdering.InBlock)
    Validator.validate(fs, state, txs, heightOpt, time.correctedTime())._2
  }

}

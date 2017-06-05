package scorex.transaction

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{ByteStr, Validator}
import scorex.block.Block
import scorex.consensus.TransactionsOrdering
import scorex.utils.Time

import scala.concurrent.duration._

trait UnconfirmedTransactionsStorage {
  def putIfNew[T <: Transaction](tx: T, txValidator: T => Either[ValidationError, T]): Either[ValidationError, T]

  def all(): Seq[Transaction]

  def getBySignature(signature: ByteStr): Option[Transaction]

  def remove(tx: Transaction)
}

object UnconfirmedTransactionsStorage {

  val MaxTimeUtxFuture: FiniteDuration = 15.seconds
  val MaxTimeUtxPast: FiniteDuration = 90.minutes

  def clearIncorrectTransactions(fs: FunctionalitySettings, stateReader: StateReader, utx: UnconfirmedTransactionsStorage, time: Time): Unit = {
    val currentTime = time.correctedTime()
    val possiblyValid = utx.all()
      .filter { tx => (currentTime - tx.timestamp).millis <= MaxTimeUtxPast }
      .filter { tx => (tx.timestamp - currentTime).millis <= MaxTimeUtxFuture }
      .sorted(TransactionsOrdering.InUTXPool)
    val valid = Validator.validate(fs, stateReader, possiblyValid, None, currentTime)._2
    utx.all().diff(valid).foreach(utx.remove)
  }

  def packUnconfirmed(state: StateReader, fs: FunctionalitySettings, utx: UnconfirmedTransactionsStorage, time: Time, height: Int): Seq[Transaction] = {
    clearIncorrectTransactions(fs, state, utx, time)
    val txs = utx.all()
      .sorted(TransactionsOrdering.InUTXPool)
      .take(Block.MaxTransactionsPerBlock)
      .sorted(TransactionsOrdering.InBlock)
    Validator.validate(fs, state, txs, Some(height), time.correctedTime())._2
  }

}

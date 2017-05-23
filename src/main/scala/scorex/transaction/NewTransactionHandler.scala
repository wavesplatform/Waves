package scorex.transaction

import akka.actor.ActorRef
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.Validator
import com.wavesplatform.state2.reader.StateReader
import scorex.utils.Time

trait NewTransactionHandler {
  def onNewTransaction[T <: Transaction](tx: T): Either[ValidationError, T]
}

class NewTransactionHandlerImpl(fs: FunctionalitySettings, networkController: ActorRef, time: Time, feeCalculator: FeeCalculator,
                                utxStorage: UnconfirmedTransactionsStorage, stateReader: StateReader)
  extends NewTransactionHandler {

  override def onNewTransaction[T <: Transaction](transaction: T): Either[ValidationError, T] =
    for {
      validAgainstFee <- feeCalculator.enoughFee(transaction)
      tx <- utxStorage.putIfNew(validAgainstFee, (t: T) => Validator.validateWithCurrentTime(fs, stateReader, time)(t))
    } yield tx
}

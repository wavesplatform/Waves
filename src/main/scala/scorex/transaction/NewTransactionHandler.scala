package scorex.transaction

import akka.actor.ActorRef
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.Validator
import com.wavesplatform.state2.reader.StateReader
import scorex.network._
import scorex.network.message.Message
import scorex.utils.ScorexLogging

import scala.util.Right

trait NewTransactionHandler {
  def onNewOffchainTransactionExcept[T <: Transaction](tx: T, exceptOf: Option[ConnectedPeer]): Either[ValidationError, T]
}

object NewTransactionHandler {

  implicit class NewTransactionHandlerExt(nth: NewTransactionHandler) {
    def onNewOffchainTransaction[T <: Transaction](tx: T): Either[ValidationError, T] = nth.onNewOffchainTransactionExcept(tx, None)
  }

}

class NewTransactionHandlerImpl(fs: FunctionalitySettings, networkController: ActorRef, feeCalculator: FeeCalculator,
                                utxStorage: UnconfirmedTransactionsStorage, history: History, stateReader: StateReader)
  extends NewTransactionHandler with ScorexLogging {

  override def onNewOffchainTransactionExcept[T <: Transaction](transaction: T, exceptOf: Option[ConnectedPeer]): Either[ValidationError, T] =
    for {
      validAgainstFee <- feeCalculator.enoughFee(transaction)
      tx <- utxStorage.putIfNew(validAgainstFee, (t: T) => Validator.validateWithHistory(history, fs, stateReader)(t))
    } yield {
      val ntwMsg = Message(TransactionalMessagesRepo.TransactionMessageSpec, Right(transaction), None)
      networkController ! NetworkController.SendToNetwork(ntwMsg, exceptOf.map(BroadcastExceptOf).getOrElse(Broadcast))
      tx
    }
}
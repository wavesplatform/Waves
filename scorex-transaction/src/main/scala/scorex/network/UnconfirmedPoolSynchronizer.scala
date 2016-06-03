package scorex.network

import scorex.app.Application
import scorex.network.NetworkController.DataFromPeer
import scorex.network.TransactionalMessagesRepo.TransactionMessageSpec
import scorex.transaction.{LagonakiTransaction, Transaction}
import scorex.utils.ScorexLogging

/**
  * Synchronizing transactions that are not in blockchain yet
  */
class UnconfirmedPoolSynchronizer(application: Application) extends ViewSynchronizer with ScorexLogging {

  override val messageSpecs = Seq(TransactionMessageSpec)

  override val networkControllerRef = application.networkController

  val transactionModule = application.transactionModule

  override def receive: Receive = {
    case DataFromPeer(msgId, tx: Transaction, remote) if msgId == TransactionMessageSpec.messageCode =>
      log.debug(s"Got tx: $tx")
      (tx, transactionModule.blockStorage.state.isValid(tx)) match {
        case (ltx: LagonakiTransaction, true) => transactionModule.utxStorage.putIfNew(ltx)
        case (atx, false) => log.error(s"Transaction $atx is not valid")
        case m => log.error(s"Got unexpected transaction: $m")
      }
  }
}
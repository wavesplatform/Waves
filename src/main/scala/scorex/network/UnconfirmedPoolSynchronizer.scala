package scorex.network

import akka.actor.{Actor, ActorRef}
import com.wavesplatform.settings.UTXSettings
import scorex.network.UnconfirmedPoolSynchronizer.BroadcastRandom
import scorex.transaction._
import scorex.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global

class UnconfirmedPoolSynchronizer(transactionModule: NewTransactionHandler, settings: UTXSettings,
                                  networkController: ActorRef, utxStorage: UnconfirmedTransactionsStorage)
  extends Actor with ScorexLogging {

  private val rndBroadcastInterval = settings.broadcastInterval

  context.system.scheduler.schedule(rndBroadcastInterval, rndBroadcastInterval, self, BroadcastRandom)

  override def receive: Receive = {
//    case DataFromPeer(msgId, tx: Transaction, remote) if msgId == TransactionMessageSpec.messageCode =>
//      log.debug(s"Got tx: $tx")
//      transactionModule.onNewTransaction(tx)
//        .left.map(err => log.error(s"Transaction $tx has been rejected by UTX pool. Reason: $err"))

    case BroadcastRandom =>
      val txs = utxStorage.all()
      if (txs.nonEmpty) {
        val rndTx = txs.toList(scala.util.Random.nextInt(txs.size))
        broadcast(rndTx)
      }
  }

  private def broadcast(tx: Transaction): Unit = {
    val spec = TransactionalMessagesRepo.TransactionMessageSpec
//    val ntwMsg = Message(spec, Right(tx), None)
//    networkController ! NetworkController.SendToNetwork(ntwMsg, Broadcast)
    log.debug(s"Unconfirmed tx has been broadcast to network: $tx")
  }

  private def broadcastExceptOf(tx: Transaction, sender: ConnectedPeer): Unit = {
//    val networkMessage = Message(TransactionalMessagesRepo.TransactionMessageSpec, Right(tx), None)
//    networkController ! NetworkController.SendToNetwork(networkMessage, BroadcastExceptOf(sender))
    log.debug(s"Unconfirmed transaction has been broadcasted to network")
  }
}

object UnconfirmedPoolSynchronizer {

  case object BroadcastRandom

}
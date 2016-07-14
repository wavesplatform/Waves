package scorex.network

import akka.actor.ActorRef
import scorex.network.NetworkController.DataFromPeer
import scorex.network.TransactionalMessagesRepo.TransactionMessageSpec
import scorex.network.UnconfirmedPoolSynchronizer.BroadcastRandom
import scorex.network.message.Message
import scorex.transaction.{LagonakiTransaction, Transaction, TransactionModule, TransactionSettings}
import scorex.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


/**
  * Synchronizing transactions that are not in blockchain yet
  */
class UnconfirmedPoolSynchronizer(private val transactionModule: TransactionModule[_],
                                  settings: TransactionSettings,
                                  networkController: ActorRef)
  extends ViewSynchronizer with ScorexLogging {

  override val messageSpecs = Seq(TransactionMessageSpec)
  override val networkControllerRef = networkController

  private val rndBroadcastInterval = settings.utxRebroadcastInterval

  override def preStart: Unit = {
    super.preStart()
    context.system.scheduler.schedule(1.second, rndBroadcastInterval, self, BroadcastRandom)
  }

  override def receive: Receive = {
    case DataFromPeer(msgId, tx: Transaction, remote) if msgId == TransactionMessageSpec.messageCode =>
      log.debug(s"Got tx: $tx")
      (tx, transactionModule.isValid(tx)) match {
        case (ltx: LagonakiTransaction, true) => if (transactionModule.putUnconfirmedIfNew(ltx)) broadcast(ltx)
        case (atx, false) => log.error(s"Transaction $atx is not valid")
        case m => log.error(s"Got unexpected transaction: $m")
      }
    case BroadcastRandom => {
      val txs = transactionModule.unconfirmedTxs()
      if (!txs.isEmpty) {
        val rndTx = txs.toList(scala.util.Random.nextInt(txs.size))
        broadcast(rndTx)
      }
    }
  }

  /**
    * Broadcast unconfirmed tx to other connected peers
    */
  private def broadcast(tx: Transaction) : Unit = {
    val spec = TransactionalMessagesRepo.TransactionMessageSpec
    val ntwMsg = Message(spec, Right(tx), None)
    networkControllerRef ! NetworkController.SendToNetwork(ntwMsg, Broadcast)
    log.debug(s"Unconfirmed tx has been broadcast to network: $tx")
  }
}

object UnconfirmedPoolSynchronizer {
  case object BroadcastRandom
}
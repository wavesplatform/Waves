package scorex.network

import akka.actor.ActorRef
import com.wavesplatform.settings.UTXSettings
import scorex.network.NetworkController.DataFromPeer
import scorex.network.TransactionalMessagesRepo.TransactionMessageSpec
import scorex.network.UnconfirmedPoolSynchronizer.BroadcastRandom
import scorex.network.message.Message
import scorex.transaction._
import scorex.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global


/**
  * Synchronizing transactions that are not in blockchain yet
  */
class UnconfirmedPoolSynchronizer(private val transactionModule: TransactionModule, settings: UTXSettings,
                                  networkController: ActorRef)
  extends ViewSynchronizer with ScorexLogging {

  override val messageSpecs = Seq(TransactionMessageSpec)
  protected override lazy val networkControllerRef = networkController

  private val rndBroadcastInterval = settings.broadcastInterval

  context.system.scheduler.schedule(rndBroadcastInterval, rndBroadcastInterval, self, BroadcastRandom)

  override def receive: Receive = {
    case DataFromPeer(msgId, tx: Transaction, remote) if msgId == TransactionMessageSpec.messageCode =>
      log.debug(s"Got tx: $tx")
      tx match {
        case ltx: TypedTransaction => if (transactionModule.putUnconfirmedIfNew(ltx)) broadcastExceptOf(ltx, remote)
        case m => log.error(s"Got unexpected transaction: $m")
      }

    case BroadcastRandom =>
      val txs = transactionModule.unconfirmedTxs
      if (txs.nonEmpty) {
        val rndTx = txs.toList(scala.util.Random.nextInt(txs.size))
        broadcast(rndTx)
      }
  }

  /**
    * Broadcast unconfirmed tx to other connected peers
    */
  private def broadcast(tx: Transaction): Unit = {
    val spec = TransactionalMessagesRepo.TransactionMessageSpec
    val ntwMsg = Message(spec, Right(tx), None)
    networkControllerRef ! NetworkController.SendToNetwork(ntwMsg, Broadcast)
    log.debug(s"Unconfirmed tx has been broadcast to network: $tx")
  }

  private def broadcastExceptOf(tx: Transaction, sender: ConnectedPeer): Unit = {
    val spec = TransactionalMessagesRepo.TransactionMessageSpec
    val networkMessage = Message(spec, Right(tx), None)
    networkControllerRef ! NetworkController.SendToNetwork(networkMessage, BroadcastExceptOf(sender))
    log.debug(s"Unconfirmed transaction has been broadcasted to network")
  }
}

object UnconfirmedPoolSynchronizer {

  case object BroadcastRandom

}
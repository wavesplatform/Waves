package scorex.network.redone

import java.net.InetSocketAddress

import akka.actor.FSM
import scorex.app.Application
import scorex.block.Block
import scorex.network.NetworkController.{DataFromPeer, SendToNetwork}
import scorex.network._
import scorex.network.message.Message
import scorex.network.redone.NetworkObject.ConsideredValue
import scorex.transaction.History
import shapeless.Typeable._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


class HistorySynchronizer(application: Application)
  extends ViewSynchronizer with FSM[HistorySynchronizer.Status, Seq[ConnectedPeer]] {

  import HistorySynchronizer._

  implicit val consensusModule = application.consensusModule
  implicit val transactionalModule = application.transactionModule

  import application.basicMessagesSpecsRepo._

  override val messageSpecs = Seq(ScoreMessageSpec, GetSignaturesSpec, SignaturesSpec, BlockMessageSpec, GetBlockSpec)

  lazy val scoreSyncer = new ScoreNetworkObject(self)

  lazy val history = application.history

  lazy val networkControllerRef = application.networkController

  lazy val blockGenerator = application.blockGenerator

  override def preStart = {
    super.preStart()
    context.system.scheduler.schedule(1.second, 1.seconds) {
      val ntwMsg = Message(ScoreMessageSpec, Right(history.score()), None)
      val stn = NetworkController.SendToNetwork(ntwMsg, SendToRandom)
      networkControllerRef ! stn
    }
  }

  application.settings.offlineGeneration match {
    case true => startWith(Synced, Seq())
    case false => startWith(ScoreNotCompared, Seq())
  }

  when(ScoreNotCompared) {
    //init signal(boxed Unit) matching
    case Event(Unit, _) =>
      stay()
  }

  when(GettingExtension, 1.minute) {
    case Event(StateTimeout, _) =>
      stay() //todo: fix

    //todo: aggregating function for block ids (like score has)
    case Event(DataFromPeer(blockIds: Seq[Block.BlockId]@unchecked, remote), _)
      if blockIds.cast[Seq[Block.BlockId]].isDefined =>
      blockIds.foreach { blockId =>
        networkControllerRef ! NetworkController.SendToNetwork(Message(GetBlockSpec, Right(blockId), None), SendToChosen(Seq(remote)))
      }
      stay()

    case Event(DataFromPeer(block: Block@unchecked, remote), _)
      if block.cast[Block].isDefined =>

      processNewBlock(block, Some(remote.address))
      stay()
  }

  //accept only new block from local or remote
  when(Synced) {
    case Event(DataFromPeer(block: Block, remote), _) =>
      processNewBlock(block, Some(remote.address))
      stay()

    case Event(block: Block, _) =>
      processNewBlock(block, None)
      stay()
  }

  //common logic for all the states
  whenUnhandled {
    case Event(DataFromPeer(content: History.BlockchainScore, remote), _) =>
      scoreSyncer.networkUpdate(remote, content)
      stay()

    case Event(ConsideredValue(Some(networkScore: History.BlockchainScore), witnesses), _) =>
      val localScore = history.score()
      if (networkScore > localScore) {
        log.info("networkScore > localScore")
        val msg = Message(GetSignaturesSpec, Right(history.lastSignatures(100)), None)
        networkControllerRef ! NetworkController.SendToNetwork(msg, SendToChosen(witnesses))
        goto(GettingExtension) using witnesses
      } else goto(Synced) using Seq() //todo: avoid goto if already in state

    case nonsense: Any =>
      log.warning(s"HistorySynchronizer: got something strange in the state:$stateName - $nonsense")
      stay()
  }

  onTransition {
    case _ -> Synced =>
      blockGenerator ! BlockGenerator.StartGeneration

    case Synced -> _ =>
      blockGenerator ! BlockGenerator.StopGeneration
  }

  initialize()

  def processNewBlock(block: Block, remoteOpt: Option[InetSocketAddress]) = {
    val fromStr = remoteOpt.map(_.toString).getOrElse("local")
    if (block.isValid) {
      log.info(s"New block: $block from $fromStr")
      application.state.processBlock(block)
      history.appendBlock(block)

      block.transactionModule.clearFromUnconfirmed(block.transactionDataField.value)

      //broadcast block only if it is generated locally
      if (remoteOpt.isEmpty) {
        val blockMsg = Message(BlockMessageSpec, Right(block), None)
        networkControllerRef ! SendToNetwork(blockMsg, Broadcast)
      }
    } else {
      log.warning(s"Non-valid block: $block from $fromStr")
    }
  }
}

object HistorySynchronizer {

  sealed trait Status

  case object ScoreNotCompared extends Status

  case object GettingExtension extends Status

  case object Synced extends Status

}

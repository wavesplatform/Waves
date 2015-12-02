package scorex.network


import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, FSM}
import scorex.app.Application
import scorex.block.Block
import scorex.network.NetworkController.DataFromPeer
import scorex.network.NetworkObject.ConsideredValue
import scorex.network.message.BasicMessagesRepo._
import scorex.network.message.{Message, MessageSpec}
import scorex.transaction.History
import scorex.transaction.History.BlockchainScore
import scorex.utils.ScorexLogging

import scala.concurrent.duration._


//extends actor?
//publish-subscribe?
//anti-ddos?

case class ListenForNetworkMessages(specs: Seq[MessageSpec[_]], listener: ActorRef)

case object UpdateNetworkView

//todo: typed actor?
//not thread-safe!!!
trait NetworkObject[V] {

  //val networkControllerRef: ActorRef
  val localComponentRef: ActorRef

  //mutable
  private var candidates = Map[ConnectedPeer, V]()

  //private var localValue: Option[V] = None

  def consider(candidates: Map[ConnectedPeer, V]): (Option[V], Seq[ConnectedPeer], Map[ConnectedPeer, V])

  def networkUpdate(remote: ConnectedPeer, value: V) = {
    candidates += remote -> value
    val ct = consider(candidates)
    candidates = ct._3

    val consideredValue = ct._1
    val witnesses = ct._2

    //todo: cache considered value and send signal only if that > previous
    localComponentRef ! ConsideredValue(consideredValue, witnesses)
  }

  /*
  override def receive = {
    case LocalUpdate(localUpdate: Option[V]) =>
      localValue = localUpdate
      localUpdate.foreach { lv =>
        networkControllerRef ! (messageSpec, sendingStrategy)
      }

    case NetworkUpdate(remote: ConnectedPeer, value: V) =>
      candidates += remote -> value
      val ct = consider(candidates)
      consideredValue = ct._1
      candidates = ct._2
      localComponentRef ! value
  } */
}


object NetworkObject {

  case class ConsideredValue[V](value: V, witnesses: Seq[ConnectedPeer])

  // case class LocalUpdate[V](localUpdate: Option[V])
  //case class NetworkUpdate[V](remote: ConnectedPeer, value: V)
}


class ScoreNetworkObject(//override val networkControllerRef: ActorRef,
                         override val localComponentRef: ActorRef) extends NetworkObject[History.BlockchainScore] {
  override def consider(candidates: Map[ConnectedPeer, BlockchainScore])
  : (Option[BlockchainScore], Map[ConnectedPeer, BlockchainScore]) = {
    val bestNetworkScore = candidates.maxBy(_._2)._2
    (Some(bestNetworkScore), candidates.filter(_._2 == bestNetworkScore))
  }
}

/*  Synchronizing network & local views of an object, e.g. history(blockchain or blocktree), known peers list,
  segments dataset subset etc.

 */
trait ViewSynchronizer extends Actor with ScorexLogging {

  protected val networkControllerRef: ActorRef

  val messageSpecs: Seq[MessageSpec[_]]
}


trait LocalHistoryProvider {
  //<-> score
  //<-> last sigs
  //<-  block
}

trait NetworkHistoryProvider {
  // -> score
  // <- get sigs
  // -> sigs
  // -> block
}


class HistorySynchronizer(application: Application)
  extends ViewSynchronizer with FSM[HistorySynchronizer.Status, Unit] {

  import HistorySynchronizer._

  override val messageSpecs = Seq(ScoreMessageSpec, GetSignaturesSpec, SignaturesSpec, BlockMessageSpec, GetBlockSpec)

  lazy val scoreSyncer = new ScoreNetworkObject(self)

  lazy val history = application.history

  startWith(ScoreNotCompared, Unit)

  when(GettingExtension, 1.minute) {
    case Event(StateTimeout, _) =>
      stay() //todo: fix

    case Event(DataFromPeer(blockIds: Seq[Block.BlockId], remote), _) =>
      blockIds.foreach { blockId =>
        networkControllerRef ! NetworkController.SendToNetwork(Message(GetBlockSpec, Right(blockId), None), SendToChosen(Seq(remote)))
      }
      stay()

    case Event(DataFromPeer(block: Block, remote), _) =>
      stay()
  }

  //accept only new block from local or remote
  when(Synced) {
    case Event(DataFromPeer(block: Block, remote), _) =>
      stay()

    case Event(block: Block, _) =>
      stay()
  }

  //common logic for all the states
  whenUnhandled {
    case Event(DataFromPeer(content: History.BlockchainScore, remote), _) =>
      scoreSyncer.networkUpdate(remote, content)
      stay()

    case Event(ConsideredValue(networkScore: History.BlockchainScore, witnesses), _) =>
      val localScore = history.score()
      if (networkScore > localScore) {
        val msg = Message(GetSignaturesSpec, Right(history.lastSignatures(100)), None)
        networkControllerRef ! NetworkController.SendToNetwork(msg, SendToChosen(witnesses))
        goto(GettingExtension)
      } else goto(Synced)
  }

  initialize()


  def processNewBlock(block: Block, remoteOpt: Option[InetSocketAddress]) = {
    val fromStr = remoteOpt.map(_.toString).getOrElse("local")
    if (block.isValid) {
      log.info(s"New block: $block from $fromStr")
      application.state.processBlock(block)
      application.history.appendBlock(block)

      block.transactionModule.clearFromUnconfirmed(block.transactionDataField.value)
      val height = application.history.height()

      //broadcast block only if it is generated locally
      if (remoteOpt.isEmpty) {
        networkController ! NetworkController.BroadcastMessage(BlockMessage(height, block))
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

//todo: get signatures <-> signatures
//todo: get block <-> block

//todo: download few chains, compare
//todo: download headers, not ids, to compare blockscore
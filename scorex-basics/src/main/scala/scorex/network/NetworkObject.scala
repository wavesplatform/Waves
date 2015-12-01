package scorex.network


import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef}
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.network.message.BasicMessagesRepo.{GetSignaturesSpec, ScoreMessageSpec, SignaturesSpec}
import scorex.network.message.{Message, MessageSpec}
import scorex.transaction.History.BlockchainScore
import scorex.transaction.{History, Transaction}
import scorex.utils.ScorexLogging

import scala.util.Try


//extends actor?
//publish-subscribe?
//anti-ddos?

case class ListenForUpdate[V](spec: MessageSpec[V], listener: ActorRef)(implicit ev:V)

case object UpdateNetworkView


trait NetworkObject[V] extends Actor with ScorexLogging {


  protected val networkControllerRef: ActorRef
  val localComponentRef: ActorRef

  val messageSpec: MessageSpec[V]
  val reqSpecOpt: Option[MessageSpec[_]]

  //mutable
  private var candidates = Map[PeerConnectionHandler, V]()
  private var consideredValue: Option[V] = None

  private var localValue: Option[V] = None

  def consider(candidates: Map[PeerConnectionHandler, V]): Option[V]

  def filterCandidates(candidates: Map[PeerConnectionHandler, V],
                       consideredValue: Option[V]): Map[PeerConnectionHandler, V]

  override def preStart = {
    val lfu = ListenForUpdate(messageSpec, self)
    networkControllerRef ! lfu
    localComponentRef ! lfu
  }

  override def receive: Receive = ({
    //update from network
    case Message(spec: MessageSpec[V], Right(value: V), Some(remote)) =>
      candidates += remote -> value
      consideredValue = consider(candidates)
      candidates = filterCandidates(candidates, consideredValue)
      localComponentRef ! value

    case localUpdate: Option[V] =>
      localValue = localUpdate

    case UpdateNetworkView =>
      reqSpecOpt match {
        case Some(reqSpec) =>
        case None =>
          log.error("No chance to ask the network")
      }
  }: Receive).orElse(additionalLogic)

  def additionalLogic: Receive
}


class NetworkScore(override val networkControllerRef: ActorRef, blockchainSyncerRef: ActorRef)
  extends NetworkObject[History.BlockchainScore] {

  override def consider(candidates: Map[PeerConnectionHandler, History.BlockchainScore]): Option[History.BlockchainScore] =
    Try(candidates.maxBy(_._2)._2).toOption

  override def filterCandidates(candidates: Map[PeerConnectionHandler, BlockchainScore],
                                consideredValue: Option[BlockchainScore]): Map[PeerConnectionHandler, BlockchainScore] = {
    candidates.filter(_._2 == consideredValue)
  }

  override val localComponentRef: ActorRef = blockchainSyncerRef
  override val reqSpecOpt = None
  override val messageSpec: MessageSpec[BlockchainScore] = ScoreMessageSpec

  override def additionalLogic: Receive = {

  }
}

//todo: get signatures <-> signatures
//todo: get block <-> block

//todo: download few chains, compare
//todo: download headers, to compare blockscore
class NetworkBlockchainExtension(override val networkControllerRef: ActorRef,
                                 blockchainSyncerRef: ActorRef) extends NetworkObject[Seq[Block.BlockId]] {
  override def consider(candidates: Map[PeerConnectionHandler, Seq[Block.BlockId]]): Option[Seq[Block.BlockId]] = {
    candidates.headOption.map(_._2)
  }

  override def filterCandidates(candidates: Map[PeerConnectionHandler, Seq[BlockId]],
                                consideredValue: Option[Seq[BlockId]]): Map[PeerConnectionHandler, Seq[BlockId]] = {
    Map[PeerConnectionHandler, Seq[BlockId]]()
  }

  override val localComponentRef: ActorRef = blockchainSyncerRef
  override val messageSpec: MessageSpec[Seq[BlockId]] = SignaturesSpec
  override val reqSpecOpt: Option[MessageSpec[_]] = Some(GetSignaturesSpec)

  override def additionalLogic: Receive = {
    case _ =>
  }
}


trait NetworkNewBlock extends NetworkObject[Block] {

}


trait NetworkKnownPeers extends NetworkObject[Seq[InetSocketAddress]] {

}

trait NetworkUnconfirmedTransactions extends NetworkObject[Seq[Transaction]] {

}

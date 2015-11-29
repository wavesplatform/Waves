package scorex.network


import java.net.InetSocketAddress

import akka.actor.Actor
import scorex.block.Block
import scorex.network.message.BasicMessagesRepo.{BlockMessageSpec, GetPeersSpec, PeersSpec, ScoreMessageSpec}
import scorex.network.message.{Message, MessageSpec}
import scorex.network.peer.PeerManager
import scorex.transaction.History

//todo: anti-ddos

trait Interaction[ReqMsgType, RespMsgType] {
  val id: Message.MessageCode

  //for a received message
  def requestHandler(reply: ReqMsgType): RespMsgType
}

trait SimplexInteraction[MsgType] extends Interaction[MsgType, Nothing] {
  val msgSpec: MessageSpec[MsgType]

  override val id = msgSpec.messageCode
}


object ScoreInteraction extends SimplexInteraction[History.BlockchainScore] {
  override val msgSpec = ScoreMessageSpec

  override def requestHandler(request: History.BlockchainScore) = ???
}

object BlockInteraction extends SimplexInteraction[Block] {
  override val msgSpec = BlockMessageSpec

  override def requestHandler(request: Block) = ???
}

trait DuplexInteraction[ReqMsg, RepMsg] extends Interaction[ReqMsg, RepMsg] {
  val reqSpec: MessageSpec[ReqMsg]
  val repSpec: MessageSpec[RepMsg]

  override val id = reqSpec.messageCode

  def responseHandler(response: RepMsg): Unit = ???
}

case class PeersInteraction(peerManager: PeerManager) extends DuplexInteraction[Unit, Seq[InetSocketAddress]] {
  override val reqSpec: MessageSpec[Unit] = GetPeersSpec

  override val repSpec: MessageSpec[Seq[InetSocketAddress]] = PeersSpec

  override def requestHandler(request: Unit): Seq[InetSocketAddress] = peerManager.knownPeers()

  override def responseHandler(peers: Seq[InetSocketAddress]): Unit = peers.foreach(peerManager.addPeer)
}


trait PeerProtocol extends Actor {
  val peerConnectionHandler: PeerConnectionHandler

  val protocol: Map[Message.MessageCode, Interaction]

  override def receive = {
    case _ => ???
  }
}
package scorex.network


import java.net.InetSocketAddress

import akka.actor.Actor
import scorex.block.Block
import scorex.network.message.BasicMessagesRepo.{PeersSpec, GetPeersSpec, BlockMessageSpec, ScoreMessageSpec}
import scorex.network.message.{Message, MessageSpec}
import scorex.network.peer.PeerManager

//todo: anti-ddos

trait Interaction[MsgType] {
  val id: Message.MessageCode

  //for a received message
  def handler(reply: MsgType): Unit
}

trait StatelessInteraction extends Interaction

trait SimplexInteraction[MsgType] extends StatelessInteraction {
  val msgSpec: MessageSpec[MsgType]

  override val id = msgSpec.messageCode
}


object ScoreInteraction extends SimplexInteraction[(Int, BigInt)] {
  override val msgSpec = ScoreMessageSpec
}

object BlockInteraction extends SimplexInteraction[Block] {
  override val msgSpec = BlockMessageSpec
}

trait StatefulInteraction extends Interaction

trait DuplexInteraction[ReqMsg, RepMsg] extends StatefulInteraction {
  val reqSpec: MessageSpec[ReqMsg]
  val repSpec: MessageSpec[RepMsg]

  override val id = reqSpec.messageCode
}

case class PeersInteraction(peerManager: PeerManager) extends DuplexInteraction[Unit, Seq[InetSocketAddress]] {
  override val reqSpec: MessageSpec[Unit] = GetPeersSpec

  override val repSpec: MessageSpec[Seq[InetSocketAddress]] = PeersSpec

  override def handler(peers: Seq[InetSocketAddress]): Unit = peers.foreach(peerManager.addPeer)
}


trait PeerProtocol extends Actor {
  val peerConnectionHandler: PeerConnectionHandler

  val protocol: Map[Message.MessageCode, Interaction]

  override def receive = {
    case _ => ???
  }
}
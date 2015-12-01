package scorex.network


import java.net.InetSocketAddress

import scorex.block.Block
import scorex.network.message.BasicMessagesRepo.{BlockMessageSpec, GetPeersSpec, PeersSpec, ScoreMessageSpec}
import scorex.network.message.{Message, MessageSpec}
import scorex.network.peer.PeerManager
import scorex.transaction.History


//todo: anti-ddos

trait Interaction[ReqMsgType, RespMsgType] {
  val reqSpec: MessageSpec[ReqMsgType]

  lazy val reqId: Message.MessageCode = reqSpec.messageCode

  lazy val respId: Message.MessageCode = reqSpec.messageCode

  //for a received message
  def requestHandler(reply: ReqMsgType): RespMsgType
}

trait ProduceableInteraction[ReqMsgType] extends Interaction[ReqMsgType, _] {
  def produce(): ReqMsgType
}

trait SimplexInteraction[MsgType] extends Interaction[MsgType, Nothing]

class ScoreInteraction(localHistory: History)
  extends SimplexInteraction[History.BlockchainScore] with ProduceableInteraction[History.BlockchainScore] {
  override val reqSpec = ScoreMessageSpec

  override def produce() = localHistory.score()

  override def requestHandler(request: History.BlockchainScore) = ???
}


object NewBlockInteraction extends SimplexInteraction[Block] {
  override val reqSpec = BlockMessageSpec

  override def requestHandler(request: Block) = ???
}

case class InteractionBox[Req1, Req2, Rep2](req: Req1,
                                          respId: Message.MessageCode,
                                          interaction: DuplexInteraction[Req2, Rep2])
                                           (implicit ev: Req1 =:= Req2)

trait DuplexInteraction[ReqMsg, RepMsg] extends Interaction[ReqMsg, RepMsg] {
  override val reqSpec: MessageSpec[ReqMsg]
  val repSpec: MessageSpec[RepMsg]

  override val respId = repSpec.messageCode

  def responseHandler(response: RepMsg): Unit = ???
}

case class PeersInteraction(peerManager: PeerManager)
  extends DuplexInteraction[Unit, Seq[InetSocketAddress]] with ProduceableInteraction[Unit] {
  override val reqSpec: MessageSpec[Unit] = GetPeersSpec

  override val repSpec: MessageSpec[Seq[InetSocketAddress]] = PeersSpec

  override def produce(): Unit = ()

  override def requestHandler(request: Unit): Seq[InetSocketAddress] = peerManager.knownPeers()

  override def responseHandler(peers: Seq[InetSocketAddress]): Unit = peers.foreach(peerManager.addPeer)
}
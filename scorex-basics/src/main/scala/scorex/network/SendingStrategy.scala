package scorex.network

import scala.util.Random

trait SendingStrategy {
  type ConnectedPeers = Seq[(Long, _)]

  def choose(connectedPeers: ConnectedPeers): ConnectedPeers
}

object SendToRandom extends SendToRandomExceptOf(1, Seq.empty)

case class SendToRandomExceptOf(maxPeersNumber: Int, exceptOf: Seq[ConnectedPeer]) extends SendingStrategy {
  override def choose(connectedPeers: ConnectedPeers): ConnectedPeers = {
    val exceptOfNonces = exceptOf.map(_.nonce).toSet
    Random.shuffle(connectedPeers.filterNot(connectedPeer => exceptOfNonces.contains(connectedPeer._1)).take(maxPeersNumber))
  }
}

case object Broadcast extends SendingStrategy {
  override def choose(connectedPeers: ConnectedPeers): ConnectedPeers = connectedPeers
}

case class SendToChosen(chosenPeers: Seq[ConnectedPeer]) extends SendingStrategy {
  override def choose(connectedPeers: ConnectedPeers): ConnectedPeers = {
    val nonces = chosenPeers.map(_.nonce).toSet
    Random.shuffle(connectedPeers.filter(connectedPeer => nonces.contains(connectedPeer._1)))
  }
}

object SendToChosen {
  def apply(peer: ConnectedPeer): SendToChosen = SendToChosen(Seq(peer))
}

case class BroadcastExceptOf(except: ConnectedPeer) extends SendingStrategy {
  override def choose(connectedPeers: ConnectedPeers): ConnectedPeers =
    if (except == null) connectedPeers else connectedPeers.filterNot(_._1 == except.nonce)
}

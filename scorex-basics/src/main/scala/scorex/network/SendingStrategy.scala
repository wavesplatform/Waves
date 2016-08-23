package scorex.network

import scala.util.Random

trait SendingStrategy {
  def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer]
}

object SendToRandom extends SendToRandomExceptOf(1, Seq.empty)

case class SendToRandomExceptOf(maxPeersNumber: Int, exceptOf: Seq[ConnectedPeer]) extends SendingStrategy {
  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] = {
    Random.shuffle(peers.filterNot(exceptOf.contains).take(maxPeersNumber))
  }
}

case object Broadcast extends SendingStrategy {
  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] = peers
}

case class SendToChosen(chosenPeers: Seq[ConnectedPeer]) extends SendingStrategy {
  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] = Random.shuffle(peers.filter(chosenPeers.contains))
}

object SendToChosen {
  def apply(connectedPeer: ConnectedPeer): SendToChosen = SendToChosen(Seq(connectedPeer))
}

case class BroadcastExceptOf(except: ConnectedPeer) extends SendingStrategy {
  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] = {
    peers.filterNot(p => p == except)
  }
}

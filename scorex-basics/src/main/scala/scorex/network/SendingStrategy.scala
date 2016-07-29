package scorex.network

import scala.util.Random

trait SendingStrategy {
  def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer]
}

object SendToRandom extends SendToRandomExceptOf(1, Seq.empty)

case class SendToRandomExceptOf(maxPeersNumber: Int, exceptOf: Seq[ConnectedPeer]) extends SendingStrategy {
  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] = {
    val filtered = peers.filterNot(exceptOf.contains)
    if (filtered.size <= maxPeersNumber) filtered else Random.shuffle(filtered).take(maxPeersNumber)
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

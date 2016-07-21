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

case class BroadcastExceptOf(exceptOf: Seq[ConnectedPeer]) extends SendingStrategy {
  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] =
    peers.filterNot(exceptOf.contains)
}

case class SendToChosen(chosenPeers: Seq[ConnectedPeer]) extends SendingStrategy {
  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] = chosenPeers
}

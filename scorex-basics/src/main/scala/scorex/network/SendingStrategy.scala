package scorex.network

import scala.util.Random

trait SendingStrategy {
  def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer]
}

object SendToRandom extends SendingStrategy {
  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] =
    if (peers.nonEmpty) Seq(peers(Random.nextInt(peers.length))) else Seq.empty
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

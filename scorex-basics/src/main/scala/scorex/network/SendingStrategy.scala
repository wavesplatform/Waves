package scorex.network

import scala.util.Random

trait SendingStrategy {
  def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer]
}

object SendToRandom extends SendingStrategy {
  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] = peers.nonEmpty match {
    case true => Seq(peers(Random.nextInt(peers.length)))
    case false => Seq()
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

case class SendToRandomFromChosen(chosenPeers: Seq[ConnectedPeer]) extends SendingStrategy {
  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] =
    Seq(chosenPeers(Random.nextInt(chosenPeers.length)))
}

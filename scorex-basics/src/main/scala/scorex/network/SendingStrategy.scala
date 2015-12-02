package scorex.network

import scala.util.Random

trait SendingStrategy {
  def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer]
}

object SendToRandom extends SendingStrategy {
  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] =
    Seq(peers(Random.nextInt(peers.length)))
}

case class Broadcast(exceptOf: Seq[ConnectedPeer]) extends SendingStrategy {
  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] =
    peers.filterNot(exceptOf.contains)
}

case class SendToChosen(chosenPeers: Seq[ConnectedPeer]) extends SendingStrategy {
  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] = chosenPeers
}
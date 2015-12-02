package scorex.network

import scala.util.Random

trait SendingStrategy {
  def choose(peers: Seq[PeerConnectionHandler]): Seq[PeerConnectionHandler]
}

object SendToRandom extends SendingStrategy {
  override def choose(peers: Seq[PeerConnectionHandler]): Seq[PeerConnectionHandler] =
    Seq(peers(Random.nextInt(peers.length)))
}

object Broadcast extends SendingStrategy {
  override def choose(peers: Seq[PeerConnectionHandler]): Seq[PeerConnectionHandler] = peers
}

case class SendToChosen(chosenPeers: Seq[PeerConnectionHandler]) extends SendingStrategy {
  override def choose(peers: Seq[PeerConnectionHandler]): Seq[PeerConnectionHandler] = chosenPeers
}
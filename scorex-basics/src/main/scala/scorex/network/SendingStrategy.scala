package scorex.network

import scala.util.Random

trait SendingStrategy {
  def choose(peers: Seq[PeerData]): Seq[PeerConnectionHandler]
}

object SendToRandom extends SendingStrategy {
  override def choose(peers: Seq[PeerData]): Seq[PeerConnectionHandler] =
    Seq(peers(Random.nextInt(peers.length)).peer)
}

object Broadcast extends SendingStrategy {
  override def choose(peers: Seq[PeerData]): Seq[PeerConnectionHandler] = peers.map(_.peer)
}

//todo: pass metadata
object BestPeer extends SendingStrategy {
  override def choose(peers: Seq[PeerData]): Seq[PeerConnectionHandler] =
    Seq(peers.maxBy(_.blockchainScore).peer)
}
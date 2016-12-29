package scorex.network

import scala.util.Random

trait SendingStrategy {
  def choose[T](connectedPeers: Seq[(Long, T)]): Seq[(Long, T)]
}

object SendToRandom extends SendingStrategy {
  override def choose[T](peers: Seq[(Long, T)]): Seq[(Long, T)] =
    if (peers.nonEmpty) Seq(peers(Random.nextInt(peers.length))) else Seq.empty
}

case object Broadcast extends SendingStrategy {
  override def choose[T](connectedPeers: Seq[(Long, T)]): Seq[(Long, T)] = connectedPeers
}

case class SendToChosen(chosenPeers: Seq[ConnectedPeer]) extends SendingStrategy {
  override def choose[T](connectedPeers: Seq[(Long, T)]): Seq[(Long, T)] = {
    val nonces = chosenPeers.map(_.nonce).toSet
    Random.shuffle(connectedPeers.filter(connectedPeer => nonces.contains(connectedPeer._1)))
  }
}

object SendToChosen {
  def apply(peer: ConnectedPeer): SendToChosen = SendToChosen(Seq(peer))
}

case class BroadcastExceptOf(except: ConnectedPeer) extends SendingStrategy {
  override def choose[T](connectedPeers: Seq[(Long, T)]): Seq[(Long, T)] =
    if (except == null) connectedPeers else connectedPeers.filterNot(_._1 == except.nonce)
}

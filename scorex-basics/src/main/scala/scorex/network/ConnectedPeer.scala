package scorex.network

import java.net.InetSocketAddress

import akka.actor.ActorRef
import scorex.network.peer.PeerManager.AddToBlacklist

trait ConnectedPeer {
  def nonce: Long
  def blacklist(): Unit
}

class InetAddressPeer(nodeNonce: Long, addr: InetSocketAddress, peerManager: ActorRef) extends ConnectedPeer {
  import shapeless.syntax.typeable._

  override def blacklist(): Unit = {
    peerManager ! AddToBlacklist(nodeNonce)
  }

  override def nonce: Long = nodeNonce

  override def equals(obj: Any): Boolean =
    obj.cast[InetAddressPeer].exists(_.nonce == this.nonce)

  override def hashCode(): Int = nonce.hashCode()

  override def toString: String = addr.toString
}

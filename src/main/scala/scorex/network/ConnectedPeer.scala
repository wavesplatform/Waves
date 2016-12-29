package scorex.network

import java.net.InetSocketAddress

import akka.actor.ActorRef
import scorex.network.peer.PeerManager

trait ConnectedPeer {
  def nonce: Long

  def blacklist(): Unit

  def suspect(): Unit
}

class InetAddressPeer(nodeNonce: Long, address: InetSocketAddress, peerManager: ActorRef) extends ConnectedPeer {

  import shapeless.syntax.typeable._

  override def blacklist(): Unit = {
    peerManager ! PeerManager.AddToBlacklist(address)
  }

  override def suspect(): Unit = {
    peerManager ! PeerManager.Suspect(address)
  }

  override def nonce: Long = nodeNonce

  override def equals(obj: Any): Boolean =
    obj.cast[InetAddressPeer].exists(_.nonce == this.nonce)

  override def hashCode(): Int = nonce.hashCode()

  override def toString: String = address.toString
}

package scorex.network.peer

import java.net.InetSocketAddress
import PeerInfo.TBD

@SerialVersionUID(-8490103514095092419L)
case class PeerInfo(lastSeen: Long = TBD,
                    nonce: Option[Long] = None,
                    nodeName: Option[String] = None,
                    creationTime: Long = TBD)

object PeerInfo {
  val TBD = -1L
}

trait PeerDatabase {

  type HostName = String

  def mergePeerInfo(peer: InetSocketAddress, peerInfo: PeerInfo, createIfNotExists: Boolean = true): Unit

  def knownPeers(forSelf: Boolean): Map[InetSocketAddress, PeerInfo]

  def blacklistedPeers: Set[HostName]

  def blacklistPeer(peer: InetSocketAddress): Unit
}


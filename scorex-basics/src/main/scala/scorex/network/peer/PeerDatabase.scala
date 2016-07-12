package scorex.network.peer

import java.net.InetSocketAddress

//todo: add optional nonce
@SerialVersionUID(-8490103514095092419L)
case class PeerInfo(lastSeen: Long,
                    nonce: Option[Long] = None,
                    nodeName: Option[String] = None,
                    blacklistingTime: Long = 0L)

trait PeerDatabase {
  def addOrUpdateKnownPeer(peer: InetSocketAddress, peerInfo: PeerInfo): Unit

  def knownPeers(forSelf: Boolean): Map[InetSocketAddress, PeerInfo]

  def blacklisted: Map[InetSocketAddress, PeerInfo]

  def blacklist(peer: InetSocketAddress): Unit

  def unBlacklist(peer: InetSocketAddress): Unit

  def isBlacklisted(address: InetSocketAddress): Boolean
}


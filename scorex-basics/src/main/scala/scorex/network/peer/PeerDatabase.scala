package scorex.network.peer

import java.net.InetSocketAddress

@SerialVersionUID(-8490103514095092419L)
case class PeerInfo(lastSeen: Long = 0L,
                    nonce: Option[Long] = None,
                    nodeName: Option[String] = None,
                    blacklistingTime: Long = 0L) {

  def isBlacklisted: Boolean = blacklistingTime > 0L
  def blacklist: PeerInfo = PeerInfo(lastSeen, nonce, nodeName, System.currentTimeMillis)
  def unBlacklist: PeerInfo = PeerInfo(lastSeen, nonce, nodeName, 0L)
}

trait PeerDatabase {
  def mergePeerInfo(peer: InetSocketAddress, peerInfo: PeerInfo, createIfNotExists: Boolean = true): Unit

  def knownPeers(forSelf: Boolean): Map[InetSocketAddress, PeerInfo]

  def blacklisted: Map[InetSocketAddress, PeerInfo]

  def blacklist(peer: InetSocketAddress): Unit

  def unBlacklist(peer: InetSocketAddress): Unit

  def isBlacklisted(address: InetSocketAddress): Boolean
}


package scorex.network.peer

import java.net.InetSocketAddress

trait PeerDatabase {
  def addKnownPeer(peer: InetSocketAddress): Unit

  def knownPeers(): Seq[InetSocketAddress]

  def blacklistPeer(peer: InetSocketAddress): Unit

  def blacklistedPeers(): Seq[InetSocketAddress]

  def isBlacklisted(address: InetSocketAddress): Boolean
}


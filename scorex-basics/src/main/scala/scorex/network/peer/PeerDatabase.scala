package scorex.network.peer

import java.net.InetSocketAddress

trait PeerDatabase {
  def addConnectedPeer(peer: InetSocketAddress): Unit

  def removeConnectedPeer(peer: InetSocketAddress): Unit

  def allConnectedPeers(peer: InetSocketAddress): Seq[InetSocketAddress]

  def addKnownPeer(peer: InetSocketAddress)

  def knownPeers(): Seq[InetSocketAddress]

  def blacklistPeer(peer: InetSocketAddress)

  def isBlacklisted(address: InetSocketAddress): Boolean
}


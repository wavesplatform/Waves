package scorex.transaction.state.database

import java.net.InetSocketAddress

import scala.collection.mutable


trait PeerDatabase {
  def addConnectedPeer(peer: InetSocketAddress): Unit

  def removeConnectedPeer(peer: InetSocketAddress): Unit

  def allConnectedPeers(peer: InetSocketAddress): Seq[InetSocketAddress]

  def addKnownPeer(peer: InetSocketAddress)

  def knownPeers(): Seq[InetSocketAddress]

  def blacklistPeer(peer: InetSocketAddress)

  def isBlacklisted(address: InetSocketAddress): Boolean
}

//todo: persistence of known & blacklisted peers
object PeerDatabaseImpl extends PeerDatabase {
  private val connected = mutable.Buffer[InetSocketAddress]()
  private val whitelist = mutable.Buffer[InetSocketAddress]()
  private val blacklist = mutable.Buffer[InetSocketAddress]()

  override def addConnectedPeer(peer: InetSocketAddress) = connected.synchronized(
    connected += peer
  )

  override def removeConnectedPeer(peer: InetSocketAddress) = connected.synchronized(
    connected -= peer
  )

  override def allConnectedPeers(peer: InetSocketAddress) = connected.synchronized(
    connected.toSeq
  )

  override def addKnownPeer(peer: InetSocketAddress): Unit = whitelist.synchronized {
    whitelist += peer
  }

  override def blacklistPeer(peer: InetSocketAddress): Unit = this.synchronized {
    whitelist -= peer
    blacklist += peer
  }

  override def isBlacklisted(address: InetSocketAddress): Boolean =
    blacklist.synchronized(blacklist.contains(address))

  override def knownPeers(): Seq[InetSocketAddress] = whitelist.synchronized(whitelist.toSeq)
}
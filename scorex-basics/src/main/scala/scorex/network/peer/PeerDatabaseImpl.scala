package scorex.network.peer

import java.net.InetSocketAddress

import scala.collection.mutable

//todo: persistence of known & blacklisted peers
object PeerDatabaseImpl extends PeerDatabase {
  private val whitelist = mutable.Buffer[InetSocketAddress]()
  private val blacklist = mutable.Buffer[InetSocketAddress]()

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

  override def blacklistedPeers(): Seq[InetSocketAddress] = blacklist.synchronized(blacklist.toSeq)
}

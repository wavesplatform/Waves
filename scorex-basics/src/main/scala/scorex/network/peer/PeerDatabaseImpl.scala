package scorex.network.peer

import java.net.InetSocketAddress

import scala.collection.concurrent.TrieMap

//todo: persistence of known & blacklisted peers
object PeerDatabaseImpl extends PeerDatabase {
  private val whitelist = TrieMap[InetSocketAddress, PeerInfo]()
  private val blacklist = TrieMap[InetSocketAddress, Long]()

  override def addOrUpdateKnownPeer(address: InetSocketAddress, peerInfo: PeerInfo): Unit = {
    val updatedPeerInfo = whitelist.get(address).map { case dbPeerInfo =>
      PeerInfo(peerInfo.lastSeen, dbPeerInfo.self | peerInfo.self)
    }.getOrElse(peerInfo)
    whitelist.update(address, updatedPeerInfo)
  }


  override def blacklistPeer(address: InetSocketAddress): Unit = this.synchronized {
    whitelist -= address
    blacklist += address -> System.currentTimeMillis()
  }

  override def isBlacklisted(address: InetSocketAddress): Boolean =
    blacklist.synchronized(blacklist.contains(address))

  override def knownPeers(forSelf: Boolean): Seq[InetSocketAddress] =
    (forSelf match {
      case true => whitelist.filter(_._2.self != true)
      case false => whitelist
    }).keys.toSeq

  override def blacklistedPeers(): Seq[InetSocketAddress] =
    blacklist.keys.toSeq
}

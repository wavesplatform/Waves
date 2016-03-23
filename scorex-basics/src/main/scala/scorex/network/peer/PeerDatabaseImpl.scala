package scorex.network.peer

import java.net.InetSocketAddress

import scorex.app.Application
import scorex.storage.MapDBStorage

import scala.collection.concurrent.TrieMap

//todo: persistence of known & blacklisted peers
class PeerDatabaseImpl(application: Application, filename: String) extends PeerDatabase {

  class PeersPersistance() extends MapDBStorage[InetSocketAddress, PeerInfo](filename)

  private val whitelistPersistence = new PeersPersistance

  private val blacklist = TrieMap[InetSocketAddress, Long]()

  private lazy val ownNonce = application.settings.nodeNonce

  override def addOrUpdateKnownPeer(address: InetSocketAddress, peerInfo: PeerInfo): Unit = {
    val updatedPeerInfo = whitelistPersistence.get(address).map { case dbPeerInfo =>
      val nonceOpt = peerInfo.nonce.orElse(dbPeerInfo.nonce)
      val nodeNameOpt = peerInfo.nodeName.orElse(dbPeerInfo.nodeName)
      PeerInfo(peerInfo.lastSeen, nonceOpt, nodeNameOpt)
    }.getOrElse(peerInfo)
    whitelistPersistence.set(address, updatedPeerInfo)
    whitelistPersistence.commit()
  }

  override def blacklistPeer(address: InetSocketAddress): Unit = this.synchronized {
    whitelistPersistence.remove(address)
    blacklist += address -> System.currentTimeMillis()
    whitelistPersistence.commit()
  }

  override def isBlacklisted(address: InetSocketAddress): Boolean =
    blacklist.synchronized(blacklist.contains(address))

  override def knownPeers(excludeSelf: Boolean): Map[InetSocketAddress, PeerInfo] =
    (excludeSelf match {
      case true => knownPeers(false).filter(_._2.nonce.getOrElse(-1) != ownNonce)
      case false =>
        whitelistPersistence.keySet().flatMap(k => whitelistPersistence.get(k).map(v => k -> v))
    }).toMap

  override def blacklistedPeers(): Seq[InetSocketAddress] =
    blacklist.keys.toSeq
}
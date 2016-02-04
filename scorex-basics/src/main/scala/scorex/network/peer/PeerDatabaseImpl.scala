package scorex.network.peer

import java.net.InetSocketAddress

import scorex.app.Application

import scala.collection.concurrent.TrieMap

//todo: persistence of known & blacklisted peers
class PeerDatabaseImpl(application: Application) extends PeerDatabase {
  private val whitelist = TrieMap[InetSocketAddress, PeerInfo]()
  private val blacklist = TrieMap[InetSocketAddress, Long]()

  private lazy val ownNonce = application.settings.nodeNonce

  override def addOrUpdateKnownPeer(address: InetSocketAddress, peerInfo: PeerInfo): Unit = {
    val updatedPeerInfo = whitelist.get(address).map { case dbPeerInfo =>
      val nonceOpt = peerInfo.nonce.orElse(dbPeerInfo.nonce)
      val nodeNameOpt = peerInfo.nodeName.orElse(dbPeerInfo.nodeName)
      PeerInfo(peerInfo.lastSeen, nonceOpt, nodeNameOpt)
    }.getOrElse(peerInfo)
    whitelist.update(address, updatedPeerInfo)
  }

  override def blacklistPeer(address: InetSocketAddress): Unit = this.synchronized {
    whitelist -= address
    blacklist += address -> System.currentTimeMillis()
  }

  override def isBlacklisted(address: InetSocketAddress): Boolean =
    blacklist.synchronized(blacklist.contains(address))

  override def knownPeers(excludeSelf: Boolean): Seq[InetSocketAddress] =
    (excludeSelf match {
      case true => whitelist.filter(_._2.nonce.getOrElse(-1) != ownNonce)
      case false => whitelist
    }).keys.toSeq

  override def blacklistedPeers(): Seq[InetSocketAddress] =
    blacklist.keys.toSeq
}

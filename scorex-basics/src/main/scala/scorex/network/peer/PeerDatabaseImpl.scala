package scorex.network.peer

import java.net.InetSocketAddress
import java.util

import org.h2.mvstore.{MVMap, MVStore}
import scorex.settings.Settings

import scala.collection.JavaConversions._

class PeerDatabaseImpl(settings: Settings, filename: Option[String]) extends PeerDatabase {


  val database = filename match {
    case Some(file) => new MVStore.Builder().fileName(file).compress().open()
    case None => new MVStore.Builder().open()
  }

  private val whitelistPersistence: MVMap[InetSocketAddress, PeerInfo] = database.openMap("whitelist")
  private val blacklist: MVMap[String, Long] = database.openMap("blacklist")

  private lazy val ownNonce = settings.nodeNonce

  override def addOrUpdateKnownPeer(address: InetSocketAddress, peerInfo: PeerInfo): Unit = {
    if (!isBlacklisted(address)) {
      val updatedPeerInfo = Option(whitelistPersistence.get(address)).map { case dbPeerInfo =>
        val nonceOpt = peerInfo.nonce.orElse(dbPeerInfo.nonce)
        val nodeNameOpt = peerInfo.nodeName.orElse(dbPeerInfo.nodeName)
        PeerInfo(peerInfo.lastSeen, nonceOpt, nodeNameOpt)
      }.getOrElse(peerInfo)
      whitelistPersistence.put(address, updatedPeerInfo)
      database.commit()
    }
  }

  override def blacklistPeer(address: InetSocketAddress): Unit = {
    if (!isBlacklisted(address)) {
      blacklist += address.getHostName -> System.currentTimeMillis()
      new util.ArrayList(whitelistPersistence.keyList())
        .filter(_.getHostName == address.getHostName)
        .foreach(whitelistPersistence.remove(_))
      database.commit()
    }
  }

  override def removeFromBlacklist(address: InetSocketAddress): Unit = {
    blacklist -= address.getHostName
    whitelistPersistence.put(address, PeerInfo(System.currentTimeMillis(), None, None))
    database.commit()
  }

  override def isBlacklisted(address: InetSocketAddress): Boolean = {
    blacklist.synchronized(blacklist.contains(address.getHostName))
  }

  override def knownPeers(excludeSelf: Boolean): Map[InetSocketAddress, PeerInfo] =
    (excludeSelf match {
      case true => knownPeers(false).filter(_._2.nonce.getOrElse(-1) != ownNonce)
      case false => whitelistPersistence.keys.flatMap(k => Option(whitelistPersistence.get(k)).map(v => k -> v))
    }).toMap

  override def blacklistedPeers(): Seq[String] = blacklist.keys.toSeq

}
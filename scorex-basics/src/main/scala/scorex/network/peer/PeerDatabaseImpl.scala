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

  private val peersPersistence: MVMap[InetSocketAddress, PeerInfo] = database.openMap("peers")

  private lazy val ownNonce = settings.nodeNonce
  private lazy val blacklistResidenceTimeMilliseconds = settings.blacklistResidenceTimeMilliseconds

  def addOrUpdateKnownPeer(address: InetSocketAddress, peerInfo: PeerInfo): Unit = {
    val updatedPeerInfo = Option(peersPersistence.get(address)).map { case dbPeerInfo =>
      val nonceOpt = peerInfo.nonce.orElse(dbPeerInfo.nonce)
      val nodeNameOpt = peerInfo.nodeName.orElse(dbPeerInfo.nodeName)
      PeerInfo(peerInfo.lastSeen, nonceOpt, nodeNameOpt, dbPeerInfo.blacklistingTime)
    }.getOrElse(peerInfo)
    peersPersistence.put(address, updatedPeerInfo)
    database.commit()
  }

  /**
    * Mark peer with address as blacklisted
    */
  def blacklist(address: InetSocketAddress): Unit = {
    Option(peersPersistence.get(address)) match {
      case Some(peer) => {
        val updatedInfo = PeerInfo(peer.lastSeen, peer.nonce, peer.nodeName, System.currentTimeMillis())
        peersPersistence.put(address, updatedInfo)
        database.commit()
      }
      case _ =>
    }
  }

  def unBlacklist(address: InetSocketAddress): Unit = {
    Option(peersPersistence.get(address)) match {
      case Some(peer) => {
        val updatedInfo = PeerInfo(peer.lastSeen, peer.nonce, peer.nodeName, 0L)
        peersPersistence.put(address, updatedInfo)
        database.commit()
      }
      case _ =>
    }
  }

  def isBlacklisted(address: InetSocketAddress): Boolean = {
    Option(peersPersistence.get(address)) match {
      case Some(peer) => {
        val current = System.currentTimeMillis
        if (peer.blacklistingTime < current - blacklistResidenceTimeMilliseconds) {
          val updatedInfo = PeerInfo(peer.lastSeen, peer.nonce, peer.nodeName, 0L)
          peersPersistence.put(address, updatedInfo)
          database.commit()
          false
        } else true
      }
      case None => false
    }
  }

  /**
    * Returns all known peers which not blacklisted
    */
  def knownPeers(excludeSelf: Boolean): Map[InetSocketAddress, PeerInfo] =
    (excludeSelf match {
      case true => knownPeers(false).filter(_._2.nonce.getOrElse(-1) != ownNonce)
      case false => notBlacklisted
    }).toMap

  def blacklisted: Map[InetSocketAddress, PeerInfo] = peersPersistence.keys
    .flatMap(k => Option(peersPersistence.get(k)).map(v => k -> v))
    .filter(p => p._2.blacklistingTime > 0L).toMap

  private def notBlacklisted = peersPersistence.keys
    .flatMap(k => Option(peersPersistence.get(k)).map(v => k -> v))
    .filter(p => p._2.blacklistingTime == 0L)

}
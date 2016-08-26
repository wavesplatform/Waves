package scorex.network.peer

import java.net.InetSocketAddress

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
  private lazy val peersDataResidenceTime = settings.peersDataResidenceTime

  def mergePeerInfo(address: InetSocketAddress, peerInfo: PeerInfo, createIfNotExists: Boolean) =
    Option(peersPersistence.get(address)).map {
      dbPeerInfo =>
        PeerInfo(
          if (peerInfo.lastSeen > 0) peerInfo.lastSeen else dbPeerInfo.lastSeen,
          peerInfo.nonce.orElse(dbPeerInfo.nonce),
          peerInfo.nodeName.orElse(dbPeerInfo.nodeName),
          dbPeerInfo.blacklistingTime)
    } orElse {
      if (createIfNotExists) Some(peerInfo.copy(lastSeen = System.currentTimeMillis())) else None
    } foreach {
      updatedPeerInfo =>
        peersPersistence.put(address, updatedPeerInfo)
        database.commit()
    }

  /**
    * Mark peer with address as blacklisted
    */
  def blacklist(address: InetSocketAddress): Unit =
    Option(peersPersistence.get(address)) foreach {
      peer =>
        peersPersistence.put(address, peer.blacklist)
        database.commit()
    }

  def unBlacklist(address: InetSocketAddress): Unit =
    Option(peersPersistence.get(address)) foreach {
      peer =>
        peersPersistence.put(address, peer.unBlacklist)
        database.commit()
    }

  def isBlacklisted(address: InetSocketAddress): Boolean =
    Option(peersPersistence.get(address)) exists {
      peer =>
        val current = System.currentTimeMillis
        if (peer.blacklistingTime <= current - blacklistResidenceTimeMilliseconds) {
          peersPersistence.put(address, peer.unBlacklist)
          database.commit()
          false
        } else true
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
    .filter(p => p._2.isBlacklisted).toMap

  private def notBlacklisted = {
    val current = System.currentTimeMillis
    val obsoletePeers = peersPersistence.toArray
      .filter(_._2.lastSeen <= current - peersDataResidenceTime.toMillis).map(_._1)

    if (obsoletePeers.nonEmpty) {
      obsoletePeers.foreach(peersPersistence.remove)
      database.commit()
    }

    peersPersistence.keys
      .flatMap(k => Option(peersPersistence.get(k)).map(v => k -> v))
      .filter(p => !p._2.isBlacklisted)
  }
}
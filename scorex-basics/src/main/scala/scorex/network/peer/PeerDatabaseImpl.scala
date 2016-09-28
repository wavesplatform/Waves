package scorex.network.peer

import java.net.InetSocketAddress

import org.h2.mvstore.{MVMap, MVStore}
import scorex.settings.Settings

import scala.collection.JavaConversions._

class PeerDatabaseImpl(settings: Settings, filename: Option[String]) extends PeerDatabase {

  private val database = filename match {
    case Some(file) => new MVStore.Builder().fileName(file).compress().open()
    case None => new MVStore.Builder().open()
  }

  private val peersPersistence: MVMap[InetSocketAddress, PeerInfo] = database.openMap("peers")
  private val blacklist: MVMap[String, Long] = database.openMap("blacklist")

  private lazy val ownNonce = settings.nodeNonce
  private lazy val blacklistResidenceTimeMilliseconds = settings.blacklistResidenceTimeMilliseconds
  private lazy val peersDataResidenceTime = settings.peersDataResidenceTime
  private lazy val maxSize = settings.maxConnections * 10

  def mergePeerInfo(address: InetSocketAddress, peerInfo: PeerInfo, createIfNotExists: Boolean): Unit =
    Option(peersPersistence.get(address)).map {
      dbPeerInfo =>
        PeerInfo(
          if (peerInfo.lastSeen != PeerInfo.TBD) peerInfo.lastSeen else dbPeerInfo.lastSeen,
          peerInfo.nonce.orElse(dbPeerInfo.nonce),
          peerInfo.nodeName.orElse(dbPeerInfo.nodeName),
          dbPeerInfo.creationTime)
    } orElse {
      if (createIfNotExists && peersPersistence.size() < maxSize) {
        val t = currentTime
        Some(peerInfo.copy(lastSeen = t, creationTime = t))
      } else None
    } foreach {
      updatedPeerInfo =>
        peersPersistence.put(address, updatedPeerInfo)
        database.commit()
    }

  def blacklistPeer(address: InetSocketAddress): Unit = {
    blacklist.update(address.getHostName, currentTime)
    database.commit()
  }

  def knownPeers(excludeSelf: Boolean): Map[InetSocketAddress, PeerInfo] =
    if (excludeSelf) {
      knownPeers(false).filterNot(_._2.nonce.contains(ownNonce))
    } else {
      val blacklist = blacklistedPeers
      withoutObsoleteRecords(peersPersistence, (peerData: PeerInfo) => peerData.lastSeen, peersDataResidenceTime.toMillis)
        .toMap.filterKeys(address => !blacklist.contains(address.getHostName))
    }

  def blacklistedPeers: Set[String] =
    withoutObsoleteRecords(blacklist, { t: Long => t }, blacklistResidenceTimeMilliseconds).keySet().toSet

  private def withoutObsoleteRecords[K, T](map: MVMap[K, T], tsExtractor: T => Long, residenceTimeInMillis: Long) = {
    val t = currentTime

    val obsoletePeers = map.toArray
      .filter { case (_, value) => tsExtractor(value) <= t - residenceTimeInMillis }
      .map(_._1)

    if (obsoletePeers.nonEmpty) {
      obsoletePeers.foreach(map.remove)
      database.commit()
    }

    map
  }

  private def currentTime = System.currentTimeMillis()
}
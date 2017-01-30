package scorex.network.peer

import java.net.InetSocketAddress
import org.h2.mvstore.{MVMap, MVStore}
import scorex.settings.Settings
import scorex.utils.{CircularBuffer, LogMVMapBuilder}
import scala.collection.JavaConverters._
import scala.util.Random

class PeerDatabaseImpl(settings: Settings, filename: Option[String]) extends PeerDatabase {

  private lazy val blacklistResidenceTimeMilliseconds = settings.blacklistResidenceTimeMilliseconds
  private lazy val peersDataResidenceTime = settings.peersDataResidenceTime

  private val database = filename match {
    case Some(file) => new MVStore.Builder().fileName(file).compress().open()
    case None => new MVStore.Builder().open()
  }

  private val peersPersistence: MVMap[InetSocketAddress, PeerInfo] = database.openMap("peers", new LogMVMapBuilder[InetSocketAddress, PeerInfo])
  private val blacklist: MVMap[String, Long] = database.openMap("blacklist", new LogMVMapBuilder[String, Long])
  private val unverifiedPeers = new CircularBuffer[InetSocketAddress](settings.MaxUnverifiedPeers)

  override def addPeer(socketAddress: InetSocketAddress, nonce: Option[Long], nodeName: Option[String]): Unit = {
    if (nonce.isDefined) {
      unverifiedPeers.remove(socketAddress)
      peersPersistence.put(socketAddress, PeerInfo(System.currentTimeMillis(), nonce.get, nodeName.getOrElse("N/A")))
      database.commit()
    } else if (!getKnownPeers.contains(socketAddress)) unverifiedPeers += socketAddress
  }

  override def removePeer(socketAddress: InetSocketAddress): Unit = {
    unverifiedPeers.remove(socketAddress)
    if (peersPersistence.asScala.contains(socketAddress)) {
      peersPersistence.remove(socketAddress)
      database.commit()
    }
  }

  override def touch(socketAddress: InetSocketAddress): Unit = {
    Option(peersPersistence.get(socketAddress)).map {
      dbPeerInfo => dbPeerInfo.copy(timestamp = System.currentTimeMillis())
    } foreach { updated =>
      peersPersistence.put(socketAddress, updated)
      database.commit()
    }
  }

  override def blacklistHost(host: String): Unit = {
    unverifiedPeers.drop(_.getHostName == host)
    blacklist.put(host, System.currentTimeMillis())
    database.commit()
  }

  override def getKnownPeers: Map[InetSocketAddress, PeerInfo] = {
    withoutObsoleteRecords(peersPersistence, (peerData: PeerInfo) => peerData.timestamp, peersDataResidenceTime.toMillis)
        .asScala.toMap.filterKeys(address => !getBlacklist.contains(address.getHostName))
  }

  override def getBlacklist: Set[String] =
    withoutObsoleteRecords(blacklist, { t: Long => t }, blacklistResidenceTimeMilliseconds).keySet().asScala.toSet

  override def getRandomPeer(excluded: Set[InetSocketAddress]): Option[InetSocketAddress] = {
    val unverifiedCandidate = if (unverifiedPeers.nonEmpty)
      Some(unverifiedPeers(Random.nextInt(unverifiedPeers.size())))
    else None

    val verifiedCandidates = getKnownPeers.keySet.diff(excluded)
    val verifiedCandidate = if (verifiedCandidates.nonEmpty)
      Some(verifiedCandidates.toSeq(Random.nextInt(verifiedCandidates.size)))
    else None

    val result = if (unverifiedCandidate.isEmpty) verifiedCandidate
    else if (verifiedCandidate.isEmpty) unverifiedCandidate
    else if (Random.nextBoolean()) verifiedCandidate
    else unverifiedCandidate

    if (result.isDefined) unverifiedPeers.remove(result.get)

    result
  }

  private def withoutObsoleteRecords[K, T](map: MVMap[K, T], timestamp: T => Long, residenceTimeInMillis: Long) = {
    val t = System.currentTimeMillis()

    val obsoletePeers = map.asScala.toArray
      .filter { case (_, value) =>
        timestamp(value) <= System.currentTimeMillis() - residenceTimeInMillis
      }.map(_._1)

    if (obsoletePeers.nonEmpty) {
      obsoletePeers.foreach(map.remove)
      database.commit()
    }

    map
  }
}

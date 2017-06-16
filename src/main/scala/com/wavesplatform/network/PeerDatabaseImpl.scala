package com.wavesplatform.network

import java.net.{InetAddress, InetSocketAddress}

import com.wavesplatform.settings.NetworkSettings
import com.wavesplatform.utils.createMVStore
import org.h2.mvstore.MVMap
import scorex.utils.{CircularBuffer, LogMVMapBuilder}

import scala.collection.JavaConverters._
import scala.util.Random

class PeerDatabaseImpl(settings: NetworkSettings) extends PeerDatabase with AutoCloseable {

  private val database = createMVStore(settings.file)

  private val peersPersistence: MVMap[InetSocketAddress, PeerDatabase.PeerInfo] =
    database.openMap("peers", new LogMVMapBuilder[InetSocketAddress, PeerDatabase.PeerInfo])
  private val blacklist: MVMap[InetAddress, Long] = database.openMap("blacklist", new LogMVMapBuilder[InetAddress, Long])
  private val unverifiedPeers = new CircularBuffer[InetSocketAddress](settings.maxUnverifiedPeers)

  for (a <- settings.knownPeers.view.map(inetSocketAddress(_, 6863))) {
    addPeer(a, None, None)
  }

  override def addPeer(socketAddress: InetSocketAddress, nonce: Option[Long], nodeName: Option[String]): Unit = {
    if (nonce.isDefined) {
      unverifiedPeers.remove(socketAddress)
      peersPersistence.put(socketAddress, PeerDatabase.PeerInfo(System.currentTimeMillis(), nonce.get, nodeName.getOrElse("N/A")))
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

  override def blacklistHost(address: InetAddress): Unit = {
    unverifiedPeers.drop(_.getAddress == address)
    blacklist.put(address, System.currentTimeMillis())
    database.commit()
  }

  override def getKnownPeers: Map[InetSocketAddress, PeerDatabase.PeerInfo] = {
    withoutObsoleteRecords(peersPersistence,
      (peerData: PeerDatabase.PeerInfo) => peerData.timestamp, settings.peersDataResidenceTime.toMillis)
      .asScala.toMap.filterKeys(address => !getBlacklist.contains(address.getAddress))
  }

  override def getBlacklist: Set[InetAddress] =
    withoutObsoleteRecords(blacklist, identity[Long], settings.blackListResidenceTime.toMillis).keySet().asScala.toSet

  override def getRandomPeer(excluded: Set[InetSocketAddress]): Option[InetSocketAddress] = {
    val blacklist = getBlacklist
    unverifiedPeers.drop(isa => blacklist(isa.getAddress))

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

    val obsoletePeers = map.asScala
      .filter { case (_, value) =>
        timestamp(value) <= t - residenceTimeInMillis
      }.keys

    if (obsoletePeers.nonEmpty) {
      obsoletePeers.foreach(map.remove)
      database.commit()
    }

    map
  }

  override def close() = database.close()
}

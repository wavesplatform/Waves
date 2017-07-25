package com.wavesplatform.network

import java.net.{InetAddress, InetSocketAddress}

import com.google.common.collect.EvictingQueue
import com.wavesplatform.settings.NetworkSettings
import com.wavesplatform.utils.createMVStore
import org.h2.mvstore.MVMap
import scorex.utils.{LogMVMapBuilder, ScorexLogging}

import scala.collection.JavaConverters._
import scala.util.Random

class PeerDatabaseImpl(settings: NetworkSettings) extends PeerDatabase with AutoCloseable with ScorexLogging {

  private val database = createMVStore(settings.file)
  private val peersPersistence = database.openMap("peers", new LogMVMapBuilder[InetSocketAddress, Long])
  private val blacklist = database.openMap("blacklist", new LogMVMapBuilder[InetAddress, Long])
  private val unverifiedPeers = EvictingQueue.create[InetSocketAddress](settings.maxUnverifiedPeers)

  for (a <- settings.knownPeers.view.map(inetSocketAddress(_, 6863))) {
    // add peers from config with max timestamp so they never get evicted from the list of known peers
    doTouch(a, Long.MaxValue)
  }

  override def addCandidate(socketAddress: InetSocketAddress): Unit = unverifiedPeers.synchronized {
    if (!peersPersistence.containsKey(socketAddress) && !unverifiedPeers.contains(socketAddress)) {
      unverifiedPeers.add(socketAddress)
    }
  }

  private def doTouch(socketAddress: InetSocketAddress, timestamp: Long): Unit = unverifiedPeers.synchronized {
    unverifiedPeers.removeIf(_ == socketAddress)
    peersPersistence.compute(socketAddress, (_, prevTs) => Option(prevTs).fold(timestamp)(_.max(timestamp)))
    database.commit()
  }

  override def touch(socketAddress: InetSocketAddress): Unit = doTouch(socketAddress, System.currentTimeMillis())

  override def blacklist(address: InetAddress): Unit = unverifiedPeers.synchronized {
    unverifiedPeers.removeIf(_.getAddress == address)
    blacklist.put(address, System.currentTimeMillis())
    database.commit()
  }

  override def knownPeers: Map[InetSocketAddress, Long] = {
    removeObsoleteRecords(peersPersistence, settings.peersDataResidenceTime.toMillis)
      .asScala.toMap.filterKeys(address => !blacklistedHosts.contains(address.getAddress))
  }

  override def blacklistedHosts: Set[InetAddress] =
    removeObsoleteRecords(blacklist, settings.blackListResidenceTime.toMillis).keySet().asScala.toSet

  override def randomPeer(excluded: Set[InetSocketAddress]): Option[InetSocketAddress] = unverifiedPeers.synchronized {
    log.trace(s"Excluding: $excluded")
    def excludeAddress(isa: InetSocketAddress) = excluded(isa) || blacklistedHosts(isa.getAddress)

    log.trace(s"Evicting queue: $unverifiedPeers")
    val unverified = Option(unverifiedPeers.peek()).filterNot(excludeAddress)
    val verified = Random.shuffle(knownPeers.keySet.diff(excluded).toSeq).headOption.filterNot(excludeAddress)

    log.trace(s"Unverified: $unverified; Verified: $verified")
    (unverified, verified) match {
      case (Some(_), v@Some(_)) => if (Random.nextBoolean()) Some(unverifiedPeers.poll()) else v
      case (Some(_), None) => Some(unverifiedPeers.poll())
      case (None, v@Some(_)) => v
      case _ => None
    }
  }

  private def removeObsoleteRecords[T](map: MVMap[T, Long], maxAge: Long) = {
    val earliestValidTs = System.currentTimeMillis() - maxAge

    map.entrySet().asScala.collect {
      case e if e.getValue < earliestValidTs => e.getKey
    }.foreach(map.remove)

    database.commit()

    map
  }

  override def close(): Unit = database.close()
}

package com.wavesplatform.network

import java.net.{InetAddress, InetSocketAddress}
import java.util.concurrent.TimeUnit

import com.google.common.cache.CacheBuilder
import com.google.common.collect.EvictingQueue
import com.wavesplatform.settings.NetworkSettings
import com.wavesplatform.utils.JsonFileStorage
import io.netty.channel.Channel
import io.netty.channel.socket.nio.NioSocketChannel
import play.api.libs.json._
import scorex.utils.ScorexLogging

import scala.collection._
import scala.collection.JavaConverters._
import scala.concurrent.duration.FiniteDuration
import scala.util.Random
import scala.util.control.NonFatal

class PeerDatabaseImpl(settings: NetworkSettings) extends PeerDatabase with ScorexLogging {

  private def cache[T <: AnyRef](timeout: FiniteDuration) = CacheBuilder.newBuilder()
    .expireAfterWrite(timeout.toMillis, TimeUnit.MILLISECONDS)
    .build[T, java.lang.Long]()

  case class Peer(hostname: String, port: Int)
  implicit val format: OFormat[Peer] = Json.format[Peer]

  private type PeersPersistenceType = Set[Peer]
  private val peersPersistence = cache[InetSocketAddress](settings.peersDataResidenceTime)
  private val blacklist = cache[InetAddress](settings.blackListResidenceTime)
  private val suspension = cache[InetAddress](settings.suspensionResidenceTime)
  private val reasons = mutable.Map.empty[InetAddress, String]
  private val unverifiedPeers = EvictingQueue.create[InetSocketAddress](settings.maxUnverifiedPeers)

  for (a <- settings.knownPeers.view.map(inetSocketAddress(_, 6863))) {
    // add peers from config with max timestamp so they never get evicted from the list of known peers
    doTouch(a, Long.MaxValue)
  }

  settings.file.filter(_.exists()).foreach(f => {
    try {
      JsonFileStorage.load[PeersPersistenceType](f.getCanonicalPath).foreach(a => touch(new InetSocketAddress(a.hostname, a.port)))
      log.info(s"${f.getName} loaded, total peers: ${peersPersistence.size}")
    }
    catch {
      case NonFatal(e) =>
        log.warn("Old or invalid version peers.dat, ignoring, starting all over from known-peers...", e)
    }
  })

  if (!settings.enableBlacklisting) {
    clearBlacklist()
  }

  override def addCandidate(socketAddress: InetSocketAddress): Boolean = unverifiedPeers.synchronized {
    val r = Option(peersPersistence.getIfPresent(socketAddress)).isEmpty && !unverifiedPeers.contains(socketAddress)
    if (r) unverifiedPeers.add(socketAddress)
    r
  }

  private def doTouch(socketAddress: InetSocketAddress, timestamp: Long): Unit = unverifiedPeers.synchronized {
    unverifiedPeers.removeIf(_ == socketAddress)
    peersPersistence.put(socketAddress, Option(peersPersistence.getIfPresent(socketAddress)).fold(timestamp)(_.toLong.max(timestamp)))
  }

  override def touch(socketAddress: InetSocketAddress): Unit = doTouch(socketAddress, System.currentTimeMillis())

  override def blacklist(address: InetAddress, reason: String): Unit = {
    if (settings.enableBlacklisting) {
      unverifiedPeers.synchronized {
        unverifiedPeers.removeIf(_.getAddress == address)
        blacklist.put(address, System.currentTimeMillis())
        reasons.put(address, reason)
      }
    }
  }

  override def suspend(address: InetAddress): Unit = {
    unverifiedPeers.synchronized {
      unverifiedPeers.removeIf(_.getAddress == address)
      suspension.put(address, System.currentTimeMillis())
    }
  }

  override def knownPeers: immutable.Map[InetSocketAddress, Long] = {
    ((x: Map[InetSocketAddress, Long]) =>
      if(settings.enableBlacklisting) x.filterKeys(address => !blacklistedHosts.contains(address.getAddress))
      else x)(peersPersistence.asMap().asScala.mapValues(_.toLong)).toMap
  }

  override def blacklistedHosts: immutable.Set[InetAddress] = blacklist.asMap().asScala.keys.toSet

  override def suspendedHosts: immutable.Set[InetAddress] = suspension.asMap().asScala.keys.toSet

  override def detailedBlacklist: immutable.Map[InetAddress, (Long, String)] = blacklist.asMap().asScala.mapValues(_.toLong)
      .map { case ((h, t)) => h -> ((t, Option(reasons(h)).getOrElse(""))) }.toMap

  override def detailedSuspended: immutable.Map[InetAddress, Long] = suspension.asMap().asScala.mapValues(_.toLong).toMap

  override def randomPeer(excluded: immutable.Set[InetSocketAddress]): Option[InetSocketAddress] = unverifiedPeers.synchronized {
    def excludeAddress(isa: InetSocketAddress) = excluded(isa) || blacklistedHosts(isa.getAddress) || suspendedHosts(isa.getAddress)
    // excluded only contains local addresses, our declared address, and external declared addresses we already have
    // connection to, so it's safe to filter out all matching candidates
    unverifiedPeers.removeIf(isa => excluded(isa))
    val unverified = Option(unverifiedPeers.peek()).filterNot(excludeAddress)
    val verified = Random.shuffle(knownPeers.keySet.diff(excluded).toSeq).headOption.filterNot(excludeAddress)

    (unverified, verified) match {
      case (Some(_), v@Some(_)) => if (Random.nextBoolean()) Some(unverifiedPeers.poll()) else v
      case (Some(_), None) => Some(unverifiedPeers.poll())
      case (None, v@Some(_)) => v
      case _ => None
    }
  }

  def clearBlacklist(): Unit = {
    blacklist.invalidateAll()
    reasons.clear()
  }

  override def close(): Unit = {
    settings.file.foreach(f => {
      log.info(s"Saving ${f.getName}, total peers: ${peersPersistence.size}")
      JsonFileStorage.save[PeersPersistenceType](knownPeers.keySet.map(i => Peer(i.getHostName, i.getPort)), f.getCanonicalPath)
    })
  }

  override def blacklistAndClose(channel: Channel, reason: String): Unit = {
    val address = channel.asInstanceOf[NioSocketChannel].remoteAddress().getAddress
    log.debug(s"Blacklisting ${id(channel)}: $reason")
    blacklist(address, reason)
    channel.close()
  }
}

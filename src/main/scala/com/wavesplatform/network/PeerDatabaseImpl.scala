package com.wavesplatform.network

import java.net.{InetAddress, InetSocketAddress}
import java.nio.charset.MalformedInputException

import com.fasterxml.jackson.databind.JsonMappingException
import com.google.common.collect.EvictingQueue
import com.wavesplatform.settings.NetworkSettings
import com.wavesplatform.utils.JsonFileStorage
import io.netty.channel.Channel
import io.netty.channel.socket.nio.NioSocketChannel
import play.api.libs.json._
import scorex.utils.ScorexLogging

import scala.collection._
import scala.collection.JavaConverters._
import scala.util.Random

class PeerDatabaseImpl(settings: NetworkSettings) extends PeerDatabase with AutoCloseable with ScorexLogging {

  implicit val inetSocketAddressRW: OFormat[InetSocketAddress] = OFormat.apply[InetSocketAddress](jsValue => {
    val j = jsValue.as[JsObject]
    JsSuccess(new InetSocketAddress((j \ "hostname").as[JsString].value, (j \ "port").as[JsNumber].value.toInt))
  }, (inetSocketAddress:InetSocketAddress) => Json.obj("hostname" -> inetSocketAddress.getHostName, "port" -> inetSocketAddress.getPort))
  implicit val setW: Writes[Traversable[InetSocketAddress]] = Writes.traversableWrites[InetSocketAddress]
  implicit val setR: AnyRef with Reads[Set[InetSocketAddress]] = Reads.traversableReads[Set, InetSocketAddress]

  private type PeersPersistenceType = Set[InetSocketAddress]
  private val peersPersistence = mutable.Map.empty[InetSocketAddress, Long].asJava
  private val blacklist = mutable.Map.empty[InetAddress, Long].asJava
  private val suspension = mutable.Map.empty[InetAddress, Long].asJava
  private val reasons = mutable.Map.empty[InetAddress, String].asJava
  private val unverifiedPeers = EvictingQueue.create[InetSocketAddress](settings.maxUnverifiedPeers)

  for (a <- settings.knownPeers.view.map(inetSocketAddress(_, 6863))) {
    // add peers from config with max timestamp so they never get evicted from the list of known peers
    doTouch(a, Long.MaxValue)
  }

  settings.file.foreach(f => {
    try {
      if(f.exists()) {
        JsonFileStorage.load[PeersPersistenceType](f.getCanonicalPath).foreach(a => touch(a))
        log.info(s"${f.getName} loaded, total peers: ${peersPersistence.size()}")
      }
    }
    catch {
      case _: MalformedInputException | _: JsonMappingException =>
        log.info("Old version peers.dat, ignoring, starting all over from known-peers...")
    }
  })

  if (!settings.enableBlacklisting) {
    clearBlacklist()
  }

  override def addCandidate(socketAddress: InetSocketAddress): Unit = unverifiedPeers.synchronized {
    if (!peersPersistence.containsKey(socketAddress) && !unverifiedPeers.contains(socketAddress)) {
      log.trace(s"Adding candidate $socketAddress")
      unverifiedPeers.add(socketAddress)
    } else {
      log.trace(s"NOT adding candidate $socketAddress")
    }
  }

  private def doTouch(socketAddress: InetSocketAddress, timestamp: Long): Unit = unverifiedPeers.synchronized {
    unverifiedPeers.removeIf(_ == socketAddress)
    peersPersistence.compute(socketAddress, (_, prevTs) => Option(prevTs).fold(timestamp)(_.max(timestamp)))
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

  override def knownPeers: scala.collection.immutable.Map[InetSocketAddress, Long] = {
    ((x: Map[InetSocketAddress, Long]) =>
      if(settings.enableBlacklisting) x.filterKeys(address => !blacklistedHosts.contains(address.getAddress))
      else x)(removeObsoleteRecords(peersPersistence, settings.peersDataResidenceTime.toMillis).asScala).toMap
  }

  override def blacklistedHosts: scala.collection.immutable.Set[InetAddress] =
    removeObsoleteRecords(blacklist, settings.blackListResidenceTime.toMillis).keySet().asScala.toSet

  override def suspendedHosts: scala.collection.immutable.Set[InetAddress] =
    removeObsoleteRecords(suspension, settings.suspensionResidenceTime.toMillis).keySet().asScala.toSet

  override def detailedBlacklist: scala.collection.immutable.Map[InetAddress, (Long, String)] =
    removeObsoleteRecords(blacklist, settings.blackListResidenceTime.toMillis)
      .asScala
      .toMap
      .map { case ((h, t)) => h -> ((t, Option(reasons.get(h)).getOrElse(""))) }

  override def detailedSuspended: scala.collection.immutable.Map[InetAddress, Long] =
    removeObsoleteRecords(suspension, settings.suspensionResidenceTime.toMillis).asScala.toMap

  override def randomPeer(excluded: scala.collection.immutable.Set[InetSocketAddress]): Option[InetSocketAddress] = unverifiedPeers.synchronized {
    //    log.trace(s"Excluding: $excluded")
    def excludeAddress(isa: InetSocketAddress) = excluded(isa) || blacklistedHosts(isa.getAddress) || suspendedHosts(isa.getAddress)

    // excluded only contains local addresses, our declared address, and external declared addresses we already have
    // connection to, so it's safe to filter out all matching candidates
    unverifiedPeers.removeIf(isa => excluded(isa))
    //    log.trace(s"Evicting queue: $unverifiedPeers")
    val unverified = Option(unverifiedPeers.peek()).filterNot(excludeAddress)
    val verified = Random.shuffle(knownPeers.keySet.diff(excluded).toSeq).headOption.filterNot(excludeAddress)

    //    log.trace(s"Unverified: $unverified; Verified: $verified")
    (unverified, verified) match {
      case (Some(_), v@Some(_)) => if (Random.nextBoolean()) Some(unverifiedPeers.poll()) else v
      case (Some(_), None) => Some(unverifiedPeers.poll())
      case (None, v@Some(_)) => v
      case _ => None
    }
  }

  private def removeObsoleteRecords[T](map: java.util.Map[T, Long], maxAge: Long): java.util.Map[T, Long] = {
    val earliestValidTs = System.currentTimeMillis() - maxAge

    map.entrySet().asScala.collect {
      case e if e.getValue < earliestValidTs => e.getKey
    }.foreach(map.remove)

    map
  }

  def clearBlacklist(): Unit = {
    blacklist.clear()
    reasons.clear()
  }

  override def close(): Unit = {
    settings.file.foreach(f => {
      log.info(s"Saving ${f.getName}, total peers: ${peersPersistence.size()}")
      JsonFileStorage.save[PeersPersistenceType](knownPeers.keySet, f.getCanonicalPath)
    })
  }

  override def blacklistAndClose(channel: Channel, reason: String): Unit = {
    val address = channel.asInstanceOf[NioSocketChannel].remoteAddress().getAddress
    log.debug(s"Blacklisting ${id(channel)}: $reason")
    blacklist(address, reason)
    channel.close()
  }
}

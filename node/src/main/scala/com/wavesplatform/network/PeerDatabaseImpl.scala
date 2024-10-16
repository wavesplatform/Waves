package com.wavesplatform.network

import com.google.common.base.Ticker
import com.google.common.cache.CacheBuilder
import com.google.common.collect.EvictingQueue
import com.wavesplatform.settings.NetworkSettings
import com.wavesplatform.utils.{JsonFileStorage, ScorexLogging}
import io.netty.channel.Channel
import io.netty.channel.socket.nio.NioSocketChannel

import java.net.{InetAddress, InetSocketAddress, URI}
import java.util.concurrent.TimeUnit
import scala.annotation.tailrec
import scala.collection.*
import scala.collection.immutable.Seq
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.*
import scala.util.Random
import scala.util.control.NonFatal

class PeerDatabaseImpl(settings: NetworkSettings, ticker: Ticker = Ticker.systemTicker()) extends PeerDatabase with AutoCloseable with ScorexLogging {
  private def cache[T <: AnyRef](timeout: FiniteDuration) =
    CacheBuilder
      .newBuilder()
      .ticker(ticker)
      .expireAfterWrite(timeout.toMillis, TimeUnit.MILLISECONDS)
      .build[T, java.lang.Long]()

  private type PeersPersistenceType = Set[String]
  private val peersPersistence = cache[InetSocketAddress](settings.peersDataResidenceTime)
  private val blacklist        = cache[InetAddress](settings.blackListResidenceTime)
  private val suspension       = cache[InetAddress](settings.suspensionResidenceTime)
  private val reasons          = mutable.Map.empty[InetAddress, String]
  private val unverifiedPeers  = EvictingQueue.create[InetSocketAddress](settings.maxUnverifiedPeers)

  private val IPAndPort = """(\d+)\.(\d+)\.(\d+)\.(\d+):(\d+)""".r

  for (f <- settings.file if f.exists && f.isFile && f.length > 0) try {
    JsonFileStorage.load[PeersPersistenceType](f.getCanonicalPath).map {
      case IPAndPort(a, b, c, d, port) =>
        addCandidate(new InetSocketAddress(InetAddress.getByAddress(Array(a, b, c, d).map(_.toInt.toByte)), port.toInt))
      case _ =>
    }

    log.info(s"Loaded ${unverifiedPeers.size} known peer(s) from ${f.getName}")
  } catch {
    case NonFatal(e) => log.info("Legacy or corrupted peers.dat, ignoring, starting all over from known-peers...", e)
  }

  override def addCandidate(socketAddress: InetSocketAddress): Boolean = unverifiedPeers.synchronized {
    val r = !socketAddress.getAddress.isAnyLocalAddress &&
      !(socketAddress.getAddress.isLoopbackAddress && settings.bindAddress.exists(_.getPort == socketAddress.getPort)) &&
      Option(peersPersistence.getIfPresent(socketAddress)).isEmpty &&
      !unverifiedPeers.contains(socketAddress)
    if (r) unverifiedPeers.add(socketAddress)
    r
  }

  private def doTouch(socketAddress: InetSocketAddress, timestamp: Long): Unit = unverifiedPeers.synchronized {
    unverifiedPeers.removeIf(_ == socketAddress)
    peersPersistence.put(socketAddress, Option(peersPersistence.getIfPresent(socketAddress)).fold(timestamp)(_.toLong.max(timestamp)))
  }

  override def touch(socketAddress: InetSocketAddress): Unit = doTouch(socketAddress, System.currentTimeMillis())

  override def blacklist(inetAddress: InetAddress, reason: String): Unit =
    if (settings.enableBlacklisting) {
      unverifiedPeers.synchronized {
        unverifiedPeers.removeIf(_.getAddress == inetAddress)
        blacklist.put(inetAddress, ticker.read())
        reasons.put(inetAddress, reason)
      }
    }

  override def suspend(socketAddress: InetSocketAddress): Unit = getAddress(socketAddress).foreach { address =>
    unverifiedPeers.synchronized {
      log.trace(s"Suspending $socketAddress")
      unverifiedPeers.removeIf(_ == socketAddress)
      suspension.put(address, System.currentTimeMillis())
    }
  }

  override def knownPeers: immutable.Map[InetSocketAddress, Long] = {
    peersPersistence.cleanUp() // run all deferred actions (expiration/listeners/etc)
    peersPersistence
      .asMap()
      .asScala
      .collect {
        case (addr, ts) if !(settings.enableBlacklisting && isBlacklisted(addr.getAddress)) => addr -> ts.toLong
      }
      .toMap
  }

  def isBlacklisted(address: InetAddress): Boolean = blacklist.asMap().containsKey(address)
  def isSuspended(address: InetAddress): Boolean   = suspension.asMap().containsKey(address)

  override def detailedBlacklist: immutable.Map[InetAddress, (Long, String)] =
    blacklist.asMap().asScala.view.mapValues(_.toLong).map { case (h, t) => h -> ((t, Option(reasons(h)).getOrElse(""))) }.toMap

  override def detailedSuspended: immutable.Map[InetAddress, Long] = suspension.asMap().asScala.view.mapValues(_.toLong).toMap

  private def resolvePeerAddress(addr: String): Seq[InetSocketAddress] = {
    val uri = new URI(s"node://$addr")
    require(uri.getPort > 0, s"invalid port ${uri.getPort}")
    InetAddress
      .getAllByName(uri.getHost)
      .view
      .map { ia =>
        new InetSocketAddress(ia, uri.getPort)
      }
      .toSeq
  }

  override def nextCandidate(excluded: immutable.Set[InetSocketAddress]): Option[InetSocketAddress] = unverifiedPeers.synchronized {
    def excludeAddress(isa: InetSocketAddress): Boolean = {
      excluded(isa) || isBlacklisted(isa.getAddress) || isSuspended(isa.getAddress)
    }

    @tailrec
    def nextUnverified(): Option[InetSocketAddress] = {
      unverifiedPeers.poll() match {
        case null => None
        case nonNull =>
          if (!excludeAddress(nonNull)) Some(nonNull) else nextUnverified()
      }
    }

    settings.knownPeers
      .flatMap(p => resolvePeerAddress(p))
      .filterNot(excludeAddress)
      .headOption
      .orElse(nextUnverified())
      .orElse(Random.shuffle(knownPeers.keySet.filterNot(excludeAddress)).headOption)
  }

  def clearBlacklist(): Unit = {
    blacklist.invalidateAll()
    reasons.clear()
  }

  override def close(): Unit = settings.file.foreach { f =>
    val rawPeers = knownPeers.keySet.map(address => s"${address.getAddress.getHostAddress}:${address.getPort}")

    log.info(s"Saving ${rawPeers.size} known peer(s) to ${f.getName}")

    JsonFileStorage.save[PeersPersistenceType](rawPeers, f.getCanonicalPath)
  }

  override def blacklistAndClose(channel: Channel, reason: String): Unit = getRemoteAddress(channel).foreach { x =>
    log.debug(s"Blacklisting ${id(channel)}: $reason")
    blacklist(x.getAddress, reason)
    channel.close()
  }

  private def getAddress(socketAddress: InetSocketAddress): Option[InetAddress] = {
    val r = Option(socketAddress.getAddress)
    if (r.isEmpty) log.debug(s"Can't obtain an address from $socketAddress")
    r
  }

  private def getRemoteAddress(channel: Channel): Option[InetSocketAddress] = channel match {
    case x: NioSocketChannel => Option(x.remoteAddress())
    case x =>
      log.debug(s"Doesn't know how to get a remoteAddress from $x")
      None
  }
}

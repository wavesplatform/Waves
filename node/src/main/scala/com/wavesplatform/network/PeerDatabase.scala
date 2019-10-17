package com.wavesplatform.network

import java.net.{InetAddress, InetSocketAddress}

import com.wavesplatform.utils.ScorexLogging
import io.netty.channel.Channel

trait PeerDatabase extends AutoCloseable {

  def addCandidate(socketAddress: InetSocketAddress): Boolean

  def touch(socketAddress: InetSocketAddress): Unit

  def blacklist(host: InetAddress, reason: String): Unit

  def knownPeers: Map[InetSocketAddress, Long]

  def blacklistedHosts: Set[InetAddress]

  def suspendedHosts: Set[InetAddress]

  def randomPeer(excluded: Set[InetSocketAddress]): Option[InetSocketAddress]

  def detailedBlacklist: Map[InetAddress, (Long, String)]

  def detailedSuspended: Map[InetAddress, Long]

  def clearBlacklist(): Unit

  def suspend(host: InetSocketAddress): Unit

  def blacklistAndClose(channel: Channel, reason: String): Unit

  def suspendAndClose(channel: Channel): Unit
}

object PeerDatabase extends ScorexLogging {

  trait NoOp extends PeerDatabase {
    override def addCandidate(socketAddress: InetSocketAddress): Boolean = true

    override def touch(socketAddress: InetSocketAddress): Unit = {}

    override def blacklist(host: InetAddress, reason: String): Unit = {}

    override def knownPeers: Map[InetSocketAddress, Long] = Map.empty

    override def blacklistedHosts: Set[InetAddress] = Set.empty

    override def randomPeer(excluded: Set[InetSocketAddress]): Option[InetSocketAddress] = None

    override def detailedBlacklist: Map[InetAddress, (Long, String)] = Map.empty

    override def clearBlacklist(): Unit = ()

    override def suspend(host: InetSocketAddress): Unit = {}

    override val suspendedHosts: Set[InetAddress] = Set.empty

    override val detailedSuspended: Map[InetAddress, Long] = Map.empty

    override def blacklistAndClose(channel: Channel, reason: String): Unit = channel.close()

    override def suspendAndClose(channel: Channel): Unit = channel.close()

    override def close(): Unit = {}
  }

  object NoOp extends NoOp

}

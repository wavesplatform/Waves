package com.wavesplatform.network

import java.net.{InetAddress, InetSocketAddress}

import io.netty.channel.Channel
import scorex.utils.ScorexLogging


trait PeerDatabase extends AutoCloseable {

  def addCandidate(socketAddress: InetSocketAddress)

  def touch(socketAddress: InetSocketAddress)

  def blacklist(host: InetAddress, reason: String)

  def knownPeers: Map[InetSocketAddress, Long]

  def blacklistedHosts: Set[InetAddress]

  def suspendedHosts: Set[InetAddress]

  def randomPeer(excluded: Set[InetSocketAddress]): Option[InetSocketAddress]

  def detailedBlacklist: Map[InetAddress, (Long, String)]

  def detailedSuspended: Map[InetAddress, Long]

  def clearBlacklist(): Unit

  def suspend(host: InetAddress): Unit

  def blacklistAndClose(channel: Channel, reason: String): Unit
}

object PeerDatabase extends ScorexLogging {

  trait NoOp extends PeerDatabase {
    override def addCandidate(socketAddress: InetSocketAddress): Unit = {}

    override def touch(socketAddress: InetSocketAddress): Unit = {}

    override def blacklist(host: InetAddress, reason: String): Unit = {}

    override def knownPeers: Map[InetSocketAddress, Long] = Map.empty

    override def blacklistedHosts: Set[InetAddress] = Set.empty

    override def randomPeer(excluded: Set[InetSocketAddress]): Option[InetSocketAddress] = None

    override def detailedBlacklist: Map[InetAddress, (Long, String)] = Map.empty

    override def clearBlacklist(): Unit = ()

    override def suspend(host: InetAddress): Unit = {}

    override val suspendedHosts: Set[InetAddress] = Set.empty

    override val detailedSuspended: Map[InetAddress, Long] = Map.empty

    override def blacklistAndClose(channel: Channel, reason: String): Unit = channel.close()

    override def close(): Unit = {}
  }

  object NoOp extends NoOp

}

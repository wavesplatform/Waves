package com.wavesplatform.network

import java.net.{InetAddress, InetSocketAddress}

import io.netty.channel.Channel

trait PeerDatabase {
  def addCandidate(socketAddress: InetSocketAddress): Boolean
  def touch(socketAddress: InetSocketAddress): Unit

  def nextCandidate(excluded: Set[InetSocketAddress]): Option[InetSocketAddress]

  def blacklist(host: InetAddress, reason: String): Unit
  def blacklistAndClose(channel: Channel, reason: String): Unit
  def isBlacklisted(address: InetAddress): Boolean
  def clearBlacklist(): Unit

  def knownPeers: Map[InetSocketAddress, Long]

  def detailedBlacklist: Map[InetAddress, (Long, String)]
  def detailedSuspended: Map[InetAddress, Long]

  def suspend(host: InetSocketAddress): Unit
}

object PeerDatabase {

  object NoOp extends PeerDatabase {
    override def addCandidate(socketAddress: InetSocketAddress): Boolean = true

    override def touch(socketAddress: InetSocketAddress): Unit = {}

    override def blacklist(host: InetAddress, reason: String): Unit = {}

    override def knownPeers: Map[InetSocketAddress, Long] = Map.empty

    override def nextCandidate(excluded: Set[InetSocketAddress]): Option[InetSocketAddress] = None

    override def detailedBlacklist: Map[InetAddress, (Long, String)] = Map.empty

    override def clearBlacklist(): Unit = ()

    override def suspend(host: InetSocketAddress): Unit = {}

    override def isBlacklisted(address: InetAddress): Boolean = false

    override val detailedSuspended: Map[InetAddress, Long] = Map.empty

    override def blacklistAndClose(channel: Channel, reason: String): Unit = channel.close()
  }
}

package com.wavesplatform.network

import java.net.{InetAddress, InetSocketAddress}

import io.netty.channel.Channel
import io.netty.channel.socket.nio.NioSocketChannel
import scorex.utils.ScorexLogging


trait PeerDatabase {
  def addCandidate(socketAddress: InetSocketAddress)

  def touch(socketAddress: InetSocketAddress)

  def blacklist(host: InetAddress, reason: String)

  def knownPeers: Map[InetSocketAddress, Long]

  def blacklistedHosts: Set[InetAddress]

  def randomPeer(excluded: Set[InetSocketAddress]): Option[InetSocketAddress]

  def detailedBlacklist: Map[InetAddress, (Long, String)]

  def clearBlacklist() : Unit
}

object PeerDatabase extends ScorexLogging {

  implicit class PeerDatabaseExt(peerDatabase: PeerDatabase) {
    def blacklistAndClose(channel: Channel, reason: String): Unit = {
      val address = channel.asInstanceOf[NioSocketChannel].remoteAddress().getAddress
      log.debug(s"Blacklisting ${id(channel)}: $reason")
      peerDatabase.blacklist(address, reason)
      channel.close()
    }
  }

  val Noop = new PeerDatabase {
    override def addCandidate(socketAddress: InetSocketAddress): Unit = {}
    override def touch(socketAddress: InetSocketAddress): Unit = {}
    override def blacklist(host: InetAddress, reason: String): Unit = {}
    override def knownPeers: Map[InetSocketAddress, Long] = Map.empty
    override def blacklistedHosts: Set[InetAddress] = Set.empty
    override def randomPeer(excluded: Set[InetSocketAddress]): Option[InetSocketAddress] = None
    override def detailedBlacklist: Map[InetAddress, (Long, String)] = Map.empty
    override def clearBlacklist(): Unit = ()
  }

}

package com.wavesplatform.network

import java.net.{InetAddress, InetSocketAddress}

import io.netty.channel.Channel
import io.netty.channel.socket.nio.NioSocketChannel
import scorex.utils.ScorexLogging


trait PeerDatabase {

  def addPeer(socketAddress: InetSocketAddress, nonce: Option[Long], name: Option[String])

  def removePeer(socketAddress: InetSocketAddress)

  def touch(socketAddress: InetSocketAddress)

  def blacklistHost(host: InetAddress)

  def getKnownPeers: Map[InetSocketAddress, PeerDatabase.PeerInfo]

  def getBlacklist: Set[InetAddress]

  def getRandomPeer(excluded: Set[InetSocketAddress]): Option[InetSocketAddress]

}

object PeerDatabase extends ScorexLogging {

  case class PeerInfo(timestamp: Long, nonce: Long, nodeName: String = "")

  implicit class PeerDatabaseExt(peerDatabase: PeerDatabase) {
    def blacklistAndClose(channel: Channel): Unit = {
      val address = channel.asInstanceOf[NioSocketChannel].remoteAddress().getAddress
      log.debug(s"Blacklisting $address")
      peerDatabase.blacklistHost(address)
      channel.close()
    }
  }
}

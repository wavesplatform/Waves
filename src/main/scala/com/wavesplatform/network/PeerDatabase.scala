package com.wavesplatform.network

import java.net.{InetAddress, InetSocketAddress}



trait PeerDatabase {

  def addPeer(socketAddress: InetSocketAddress, nonce: Option[Long], name: Option[String])

  def removePeer(socketAddress: InetSocketAddress)

  def touch(socketAddress: InetSocketAddress)

  def blacklistHost(host: InetAddress)

  def getKnownPeers: Map[InetSocketAddress, PeerDatabase.PeerInfo]

  def getBlacklist: Set[InetAddress]

  def getRandomPeer(excluded: Set[InetSocketAddress]): Option[InetSocketAddress]

}

object PeerDatabase {
  case class PeerInfo(timestamp: Long, nonce: Long, nodeName: String = "")
}
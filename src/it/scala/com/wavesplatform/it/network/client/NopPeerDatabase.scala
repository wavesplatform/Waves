package com.wavesplatform.it.network.client

import java.net.{InetAddress, InetSocketAddress}

import com.wavesplatform.network.PeerDatabase

import scala.util.Random

class NopPeerDatabase extends PeerDatabase {
  override def addCandidate(socketAddress: InetSocketAddress): Unit = {}
  override def touch(socketAddress: InetSocketAddress): Unit = {}
  override def blacklist(host: InetAddress): Unit = {}
  override def knownPeers: Map[InetSocketAddress, Long] = ???
  override def blacklistedHosts: Set[InetAddress] = ???
  override def randomPeer(excluded: Set[InetSocketAddress]): Option[InetSocketAddress] = ???
}

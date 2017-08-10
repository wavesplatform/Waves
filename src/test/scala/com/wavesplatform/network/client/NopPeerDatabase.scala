package com.wavesplatform.network.client

import java.net.{InetAddress, InetSocketAddress}

import com.wavesplatform.network.PeerDatabase

object NopPeerDatabase extends PeerDatabase {
  override def addCandidate(socketAddress: InetSocketAddress): Unit = {}
  override def touch(socketAddress: InetSocketAddress): Unit = {}
  override def blacklist(host: InetAddress): Unit = {}
  override def knownPeers: Map[InetSocketAddress, Long] = Map.empty
  override def blacklistedHosts: Set[InetAddress] = Set.empty
  override def randomPeer(excluded: Set[InetSocketAddress]): Option[InetSocketAddress] = None
}

package com.wavesplatform.it.network.client

import java.net.{InetAddress, InetSocketAddress}

import com.wavesplatform.network.PeerDatabase

import scala.util.Random

object InMemoryPeerDatabase {
  private val r = new Random()
}

class InMemoryPeerDatabase extends PeerDatabase {
  import InMemoryPeerDatabase._

  private var peers = Map.empty[InetSocketAddress, Long]
  private var blacklist = Set.empty[InetAddress]

  override def addPeer(socketAddress: InetSocketAddress, nonce: Option[Long], name: Option[String]): Unit = ???

  override def removePeer(socketAddress: InetSocketAddress): Unit = ???

  override def touch(socketAddress: InetSocketAddress): Unit = ???

  override def blacklistHost(host: InetAddress): Unit = blacklist += host

  override def getKnownPeers: Map[InetSocketAddress, Long] = peers

  override def getBlacklist: Set[InetAddress] = blacklist

  override def getRandomPeer(excluded: Set[InetSocketAddress]): Option[InetSocketAddress] = {
    val afterExclusion = (peers.keys.toSet -- excluded).toIndexedSeq
    afterExclusion.headOption.map(_ =>
      afterExclusion(r.nextInt(afterExclusion.size))
    )
  }
}
